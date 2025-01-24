//! Helper types for dealing with signed JSON objects.

use crate::canonical::{CanonicalWrapper, JsonRelaxed, JsonStrict};

use super::canonical::Canonical;

use std::collections::BTreeMap;
use std::convert::TryInto;
use std::ops::Deref;

use anyhow::{Context, Error};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use serde::de::{Deserialize, DeserializeOwned, Deserializer, Error as _};
use serde::ser::Serializer;
use serde::Serialize;
use serde_json::Value;

/// API to wrap an existing signed JSON value with it's canonical form.
pub trait Wrap<V, U, T>
where
    V: Serialize,
{
    /// Wraps an existing value, with no signatures and a default unsigned
    /// section.
    fn wrap(value: V) -> Result<Signed<V, U, T>, Error>;

    /// Wraps an existing value, with no signatures and a default unsigned
    /// section.
    fn wrap_with_unsigned(value: V, unsigned: U) -> Result<Signed<V, U, T>, Error>;
}

/// A wrapper type around a deserialized signed JSON blob.
///
/// A type wrapped in [`Signed`] cannot be modified, except for the signatures
/// and unsigned fields.
///
/// If the unsigned type serializes to `null` or an empty JSON object then it
/// will be omitted during serialization. Similarly, the signatures field wil be
/// omitted if there are no signatures.
///
/// **Note**: The wrapped type should *not* have a `signatures` or `unsigned`
/// field, as they will get ignored in favour of the ones stored in the
/// [`Signed`] object.
#[derive(Clone, Debug)]
pub struct Signed<V, U = Value, T = JsonStrict> {
    value: Canonical<V, T>,

    signatures: BTreeMap<String, BTreeMap<String, Base64Signature>>,
    unsigned: U,
}

impl<V, U> Wrap<V, U, JsonStrict> for Signed<V, U, JsonStrict>
where
    V: Serialize,
    U: Default,
{
    fn wrap(value: V) -> Result<Signed<V, U, JsonStrict>, Error> {
        Ok(Signed {
            value: Canonical::<V, JsonStrict>::wrap(value)?,
            signatures: BTreeMap::new(),
            unsigned: U::default(),
        })
    }

    fn wrap_with_unsigned(value: V, unsigned: U) -> Result<Signed<V, U, JsonStrict>, Error> {
        Ok(Signed {
            value: Canonical::<V, JsonStrict>::wrap(value)?,
            signatures: BTreeMap::new(),
            unsigned,
        })
    }
}

impl<V, U> Wrap<V, U, JsonRelaxed> for Signed<V, U, JsonRelaxed>
where
    V: Serialize,
    U: Default,
{
    fn wrap(value: V) -> Result<Signed<V, U, JsonRelaxed>, Error> {
        Ok(Signed {
            value: Canonical::<V, JsonRelaxed>::wrap(value)?,
            signatures: BTreeMap::new(),
            unsigned: U::default(),
        })
    }

    fn wrap_with_unsigned(value: V, unsigned: U) -> Result<Signed<V, U, JsonRelaxed>, Error> {
        Ok(Signed {
            value: Canonical::<V, JsonRelaxed>::wrap(value)?,
            signatures: BTreeMap::new(),
            unsigned,
        })
    }
}

impl<V, U, T> Signed<V, U, T> {
    /// Unwrap signed object into the wrapped value, the unsigned part and the
    /// canonical bytes.
    pub fn into_parts(self) -> (V, U, String) {
        let (value, canonical) = self.value.into_parts();
        (value, self.unsigned, canonical)
    }

    /// Get the current set of signatures.
    pub fn signatures(&self) -> &BTreeMap<String, BTreeMap<String, Base64Signature>> {
        &self.signatures
    }

    /// Get a mutable reference to the current set of signatures.
    pub fn signatures_mut(&mut self) -> &mut BTreeMap<String, BTreeMap<String, Base64Signature>> {
        &mut self.signatures
    }

    /// Get the unsigned object
    pub fn unsigned(&self) -> &U {
        &self.unsigned
    }

    /// Get a mutable reference to the unsigned object
    pub fn unsigned_mut(&mut self) -> &mut U {
        &mut self.unsigned
    }

    /// Get the canonical JSON representation of the wrapped value, without
    /// signatures or unsigned section.
    pub fn get_canonical(&self) -> &str {
        self.value.get_canonical()
    }

    /// Add a pre-calculated signature.
    pub fn add_signature(&mut self, server_name: String, key_name: String, signature: Signature) {
        self.signatures
            .entry(server_name)
            .or_default()
            .insert(key_name, Base64Signature(signature));
    }

    /// Sign the canonical JSON and add the signature.
    pub fn sign(&mut self, server_name: String, key_name: String, key: &SigningKey) {
        let sig = self.sign_detached(key);
        self.add_signature(server_name, key_name, sig);
    }

    /// Sign the canonical JSON without adding it as the signature.
    pub fn sign_detached(&self, key: &SigningKey) -> Signature {
        let canonical = self.get_canonical();

        key.sign(canonical.as_bytes())
    }

    /// Verify the signature of the given key.
    pub fn verify_signature(
        &self,
        server_name: &str,
        key_name: &str,
        key: &VerifyingKey,
    ) -> Result<(), Error> {
        let signature = self
            .signatures
            .get(server_name)
            .and_then(|m| m.get(key_name))
            .context("missing key")?;

        key.verify(self.get_canonical().as_bytes(), signature)?;

        Ok(())
    }
}

impl<V, U> AsRef<V> for Signed<V, U> {
    fn as_ref(&self) -> &V {
        &self.value
    }
}

impl<V, U> Deref for Signed<V, U> {
    type Target = V;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<'de, V, U> Deserialize<'de> for Signed<V, U, JsonStrict>
where
    V: DeserializeOwned,
    U: DeserializeOwned + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Signed<V, U, JsonStrict>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let mut value = serde_json::Value::deserialize(deserializer)?;

        let map = value.as_object_mut().unwrap();

        let (signatures, unsigned) = extract_unsigned::<D, _>(map)?;

        let canonical = serde_json::from_value(value).map_err(serde::de::Error::custom)?;

        Ok(Signed {
            value: canonical,
            signatures,
            unsigned,
        })
    }
}

impl<'de, V, U> Deserialize<'de> for Signed<V, U, JsonRelaxed>
where
    V: DeserializeOwned,
    U: DeserializeOwned + Default,
{
    fn deserialize<D>(deserializer: D) -> Result<Signed<V, U, JsonRelaxed>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let mut value = serde_json::Value::deserialize(deserializer)?;

        let map = value.as_object_mut().unwrap();

        let (signatures, unsigned) = extract_unsigned::<D, _>(map)?;

        let canonical = serde_json::from_value(value).map_err(serde::de::Error::custom)?;

        Ok(Signed {
            value: canonical,
            signatures,
            unsigned,
        })
    }
}

fn extract_unsigned<'de, D, U>(
    map: &mut serde_json::Map<String, Value>,
) -> Result<(BTreeMap<String, BTreeMap<String, Base64Signature>>, U), D::Error>
where
    D: Deserializer<'de>,
    U: DeserializeOwned + Default,
{
    // TODO: Better error messages when this fails.

    let raw_sigs = map
        .remove("signatures")
        .unwrap_or_else(|| Value::Object(Default::default()));
    let raw_unsigned = map
        .remove("unsigned")
        .unwrap_or_else(|| Value::Object(Default::default()));

    let signatures: BTreeMap<String, BTreeMap<String, Base64Signature>> =
        serde_json::from_value(raw_sigs).map_err(|err| {
            serde::de::Error::custom(format!("Failed to parse signature field: {}", err))
        })?;

    let unsigned: U = serde_json::from_value(raw_unsigned).map_err(|err| {
        serde::de::Error::custom(format!("Failed to parse unsigned field: {}", err))
    })?;

    Ok((signatures, unsigned))
}

impl<V, U, T> Serialize for Signed<V, U, T>
where
    U: Serialize,
{
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let mut v = serde_json::to_value(&self.value).map_err(serde::ser::Error::custom)?;
        let s = serde_json::to_value(&self.signatures).map_err(serde::ser::Error::custom)?;
        let u = serde_json::to_value(&self.unsigned).map_err(serde::ser::Error::custom)?;

        v.as_object_mut()
            .unwrap()
            .insert("signatures".to_string(), s);

        if !u.is_null() && u.as_object().map(|m| !m.is_empty()).unwrap_or(true) {
            v.as_object_mut().unwrap().insert("unsigned".to_string(), u);
        }

        v.serialize(serializer)
    }
}

/// A wrapper around [`Signature`] that (de)serializes it as an unpadded base64
/// string.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Base64Signature(Signature);

impl From<Signature> for Base64Signature {
    fn from(sig: Signature) -> Base64Signature {
        Base64Signature(sig)
    }
}

impl From<Base64Signature> for Signature {
    fn from(sig: Base64Signature) -> Signature {
        sig.0
    }
}

impl AsRef<Signature> for Base64Signature {
    fn as_ref(&self) -> &Signature {
        &self.0
    }
}

impl Deref for Base64Signature {
    type Target = Signature;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl serde::Serialize for Base64Signature {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&base64::encode_config(
            self.0.to_bytes(),
            base64::STANDARD_NO_PAD,
        ))
    }
}

const B64_CONFIG: base64::Config =
    base64::Config::new(base64::CharacterSet::Standard, false).decode_allow_trailing_bits(true);

impl<'de> serde::Deserialize<'de> for Base64Signature {
    fn deserialize<D>(deserializer: D) -> Result<Base64Signature, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let de_string: String = String::deserialize(deserializer)?;

        let slice = base64::decode_config(&de_string, B64_CONFIG)
            .map_err(|e| D::Error::custom(format_args!("invalid base64: {}, {}", de_string, e,)))?;

        let sig = slice
            .as_slice()
            .try_into()
            .map_err(|_| D::Error::custom("signature incorrect length"))?;

        Ok(Base64Signature(sig))
    }
}

#[cfg(test)]
mod tests {
    use crate::keypair_from_secret_bytes;

    use super::*;
    use serde::{Deserialize, Serialize};

    #[test]
    fn base64_serialize() {
        let sig_bytes = b"_k{\x8c\xdd#h\x9b\"ejy\xed\xd6\xbd\x1a\xa9\x90\xf3\xbe\x10\x15\xbb\xa4\x08\xc4\xaas\x95\\\x95\xa0~\xda~\"\xf0\xb3\xdcd9\x03\xeb\xe7\xf3\x83\x8bd~\x94\xac\x88\x80\xe8\x82F8\x1dk\xf5rq\xa1\x02";
        let sig = Signature::from_bytes(sig_bytes);
        let b64 = Base64Signature(sig);
        let serialized = serde_json::to_string(&b64).unwrap();

        assert_eq!(
            serialized,
            r#""X2t7jN0jaJsiZWp57da9GqmQ874QFbukCMSqc5VclaB+2n4i8LPcZDkD6+fzg4tkfpSsiIDogkY4HWv1cnGhAg""#
        );
    }

    #[test]
    fn base64_deserialize() {
        let serialized = r#""X2t7jN0jaJsiZWp57da9GqmQ874QFbukCMSqc5VclaB+2n4i8LPcZDkD6+fzg4tkfpSsiIDogkY4HWv1cnGhAg""#;

        let sig_bytes = b"_k{\x8c\xdd#h\x9b\"ejy\xed\xd6\xbd\x1a\xa9\x90\xf3\xbe\x10\x15\xbb\xa4\x08\xc4\xaas\x95\\\x95\xa0~\xda~\"\xf0\xb3\xdcd9\x03\xeb\xe7\xf3\x83\x8bd~\x94\xac\x88\x80\xe8\x82F8\x1dk\xf5rq\xa1\x02";
        let expected_sig = Base64Signature(Signature::from_bytes(sig_bytes));

        let de_sig: Base64Signature = serde_json::from_str(serialized).unwrap();

        assert_eq!(de_sig, expected_sig);
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct A {
        a: i64,
    }

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct B {
        b: Option<i64>,
    }

    #[test]
    fn signed_deserialize() {
        let s: Signed<A> =
            serde_json::from_str(r#"{ "a": 1, "b": 2, "signatures": {}, "unsigned": {} }"#)
                .unwrap();

        assert_eq!(s.value.as_ref(), &A { a: 1 });
        assert_eq!(s.signatures.len(), 0);
        assert_eq!(s.value.get_canonical(), r#"{"a":1,"b":2}"#);

        let _s = serde_json::from_str::<Signed<A>>(
            r#"{ "a": 9223372036854775807, "b": 2, "signatures": {}, "unsigned": {} }"#,
        )
        .unwrap_err();

        let s: Signed<A, Value, JsonRelaxed> = serde_json::from_str(
            r#"{ "a": 9223372036854775807, "b": 2, "signatures": {}, "unsigned": {} }"#,
        )
        .unwrap();

        assert_eq!(
            s.value.as_ref(),
            &A {
                a: 9223372036854775807
            }
        );
        assert_eq!(s.signatures.len(), 0);
        assert_eq!(
            s.value.get_canonical(),
            r#"{"a":9223372036854775807,"b":2}"#
        );
    }

    #[test]
    fn signed_serialize() {
        let s = Signed::<_, serde_json::Value>::wrap(A { a: 1 }).unwrap();

        let j = serde_json::to_string(&s).unwrap();

        assert_eq!(j, r#"{"a":1,"signatures":{}}"#);

        let _s = Signed::<_, serde_json::Value>::wrap(A { a: i64::MAX }).unwrap_err();

        let s = Signed::<_, serde_json::Value, JsonRelaxed>::wrap(A { a: i64::MAX }).unwrap();

        let j = serde_json::to_string(&s).unwrap();

        assert_eq!(j, r#"{"a":9223372036854775807,"signatures":{}}"#);
    }

    #[test]
    fn signed_roundtrip() {
        let s: Signed<B> =
            serde_json::from_str(r#"{ "a": 1, "signatures": {}, "unsigned": {"test": 1} }"#)
                .unwrap();

        assert_eq!(s.value.as_ref(), &B { b: None });
        assert_eq!(s.signatures.len(), 0);
        assert_eq!(s.value.get_canonical(), r#"{"a":1}"#);

        let j = serde_json::to_string(&s).unwrap();
        assert_eq!(j, r#"{"a":1,"signatures":{},"unsigned":{"test":1}}"#);
    }

    #[test]
    fn verify() {
        let b = r#"{"signatures": {"Alice": {"ed25519:zxcvb": "hvA+XXFEkHk80pLMeIYjNkWy5Ds2ZckSrvj00NvbyFJQe3H9LuJNnu8JLZ/ffIzChs3HmhwPldO0MSmyJAYpCA"}}, "my_key": "my_data"}"#;

        #[derive(Debug, Deserialize, PartialEq, Eq)]
        struct Test {
            my_key: String,
        }

        let s: Signed<Test> = serde_json::from_str(b).unwrap();

        assert_eq!(
            s.as_ref(),
            &Test {
                my_key: "my_data".into()
            }
        );

        let k = b"qA\xeb\xc2^+(\\~P\x91(\xa4\xf4L\x1f\xeb\x07E\xae\x8b#q(\rMq\xf2\xc9\x8f\xe1\xca";

        let secret_key = SigningKey::from_bytes(k);
        let public_key = (&secret_key).into();

        s.verify_signature("Alice", "ed25519:zxcvb", &public_key)
            .unwrap();
    }

    #[test]
    fn sign() {
        #[derive(Debug, Serialize, PartialEq, Eq)]
        struct Test {
            my_key: String,
        }

        let mut s: Signed<Test, serde_json::value::Value> = Signed::wrap(Test {
            my_key: "my_data".to_string(),
        })
        .unwrap();

        assert_eq!(
            s.as_ref(),
            &Test {
                my_key: "my_data".into()
            }
        );

        let k = b"qA\xeb\xc2^+(\\~P\x91(\xa4\xf4L\x1f\xeb\x07E\xae\x8b#q(\rMq\xf2\xc9\x8f\xe1\xca";
        let keypair = keypair_from_secret_bytes(k).unwrap();

        s.sign("Alice".to_string(), "ed25519:zxcvb".to_string(), &keypair);

        let b = r#"{"my_key":"my_data","signatures":{"Alice":{"ed25519:zxcvb":"hvA+XXFEkHk80pLMeIYjNkWy5Ds2ZckSrvj00NvbyFJQe3H9LuJNnu8JLZ/ffIzChs3HmhwPldO0MSmyJAYpCA"}}}"#;
        assert_eq!(serde_json::to_string(&s).unwrap(), b);
    }
}
