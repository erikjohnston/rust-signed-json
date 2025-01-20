use serde::de::{Deserialize, DeserializeOwned, Deserializer};
use serde::ser::{Serialize, Serializer};
use serde_json::error::Error;
use std::convert::AsRef;
use std::marker::PhantomData;
use std::ops::Deref;

use crate::json::canonicalize_deserializer;
use crate::{to_string_canonical, CanonicalizationOptions};

#[derive(Clone, Debug, Default)]
pub struct JsonRelaxed;
#[derive(Clone, Debug, Default)]
pub struct JsonStrict;

pub trait CanonicalWrapper<V, T>
where
    V: Serialize,
{
    fn wrap(value: V) -> Result<Canonical<V, T>, Error>;
}

/// A read only wrapper type for deserializing a JSON value that serializes back
/// into the original JSON string (modulo being canonicalized).
///
/// This is useful if the deserialization is lossy, but the serialization needs
/// to be lossless. For example:
///
/// ```
/// use serde::Deserialize;
/// use signed_json::Canonical;
///
/// #[derive(Deserialize)]
/// struct Test {
///     a: u8,
/// }
///
/// let json = r#"{"a":1,"b":2}"#;
///
/// let canonical: Canonical<Test> = serde_json::from_str(json)?;
///
/// // We can access `a` as its part of `Test`
/// assert_eq!(canonical.a, 1);
///
/// // The original JSON is stored (in canonicalized form) and is used when
/// // serializing
/// assert_eq!(canonical.get_canonical(), json);
/// assert_eq!(serde_json::to_string(&canonical)?, json);
///
/// # Ok::<(), std::io::Error>(())
/// ```
///
/// [`Canonical`] stores the serialized canonical JSON and exposes it as
/// `get_canonical()`.
#[derive(Clone, Debug)]
pub struct Canonical<V, T = JsonStrict> {
    value: V,
    canonical_json: String,

    canonical_strictness: PhantomData<T>,
}

impl<V> CanonicalWrapper<V, JsonStrict> for Canonical<V, JsonStrict>
where
    V: Serialize,
{
    /// Wrap an existing value and use its serialization as the canonical JSON.
    fn wrap(value: V) -> Result<Canonical<V, JsonStrict>, Error> {
        let val: serde_json::Value = serde_json::to_value(&value)?;

        let canonical_json = to_string_canonical(&val, CanonicalizationOptions::strict())?;

        Ok(Canonical::<V, JsonStrict> {
            value,
            canonical_json,
            canonical_strictness: PhantomData,
        })
    }
}

impl<V> CanonicalWrapper<V, JsonRelaxed> for Canonical<V, JsonRelaxed>
where
    V: Serialize,
{
    /// Wrap an existing value and use its serialization as the canonical JSON.
    fn wrap(value: V) -> Result<Canonical<V, JsonRelaxed>, Error> {
        let val: serde_json::Value = serde_json::to_value(&value)?;

        let canonical_json = to_string_canonical(&val, CanonicalizationOptions::relaxed())?;

        Ok(Canonical::<V, JsonRelaxed> {
            value,
            canonical_json,
            canonical_strictness: PhantomData,
        })
    }
}

impl<V, T> Canonical<V, T> {
    /// Get the stored canonical JSON representation of the wrapped value.
    pub fn get_canonical(&self) -> &str {
        &self.canonical_json
    }

    /// Returns the stored canonical JSON representation of the wrapped value.
    pub fn into_canonical(self) -> String {
        self.canonical_json
    }

    /// Unwraps into the stored value and canonical JSON representation.
    pub fn into_parts(self) -> (V, String) {
        (self.value, self.canonical_json)
    }
}

impl<V, T> AsRef<V> for Canonical<V, T> {
    fn as_ref(&self) -> &V {
        &self.value
    }
}

impl<V, T> Deref for Canonical<V, T> {
    type Target = V;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<V, T> Serialize for Canonical<V, T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // We could use `RawValue` here, except that the `CanonicalSerializer`
        // forbids the use of `RawValue` for safety reasons.
        let mut deserializer = serde_json::Deserializer::from_str(&self.canonical_json);
        serde_transcode::transcode(&mut deserializer, serializer)
    }
}

impl<'de, V> Deserialize<'de> for Canonical<V, JsonStrict>
where
    V: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Canonical<V, JsonStrict>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let canonical_json =
            canonicalize_deserializer(deserializer, CanonicalizationOptions::strict())
                .map_err(serde::de::Error::custom)?;

        let value = serde_json::from_str(&canonical_json).map_err(serde::de::Error::custom)?;

        Ok(Canonical {
            value,
            canonical_json,
            canonical_strictness: PhantomData,
        })
    }
}

impl<'de, V> Deserialize<'de> for Canonical<V, JsonRelaxed>
where
    V: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Canonical<V, JsonRelaxed>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let canonical_json =
            canonicalize_deserializer(deserializer, CanonicalizationOptions::relaxed())
                .map_err(serde::de::Error::custom)?;

        let value = serde_json::from_str(&canonical_json).map_err(serde::de::Error::custom)?;

        Ok(Canonical {
            value,
            canonical_json,
            canonical_strictness: PhantomData,
        })
    }
}

#[cfg(test)]
mod tests {
    use std::i64;

    use crate::CanonicalizationOptions;

    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct A {
        a: i64,
    }

    #[test]
    fn test_deserialize() {
        let c: Canonical<A> = serde_json::from_str(r#"{"a": 2}"#).unwrap();

        assert_eq!(c.as_ref(), &A { a: 2 });

        let _c = serde_json::from_str::<Canonical<A>>(r#"{"a": 9223372036854775807}"#).unwrap_err();

        let c: Canonical<A, JsonRelaxed> =
            serde_json::from_str(r#"{"a": 9223372036854775807}"#).unwrap();

        assert_eq!(
            c.as_ref(),
            &A {
                a: 9223372036854775807
            }
        );
    }

    #[test]
    fn test_serialize() {
        let c = Canonical::<A>::wrap(A { a: 2 }).unwrap();

        let s = to_string_canonical(&c, CanonicalizationOptions::strict()).unwrap();

        assert_eq!(&s, r#"{"a":2}"#);

        let _c = Canonical::<A>::wrap(A {
            a: 9223372036854775807,
        })
        .unwrap_err();

        let c = Canonical::<A, JsonRelaxed>::wrap(A {
            a: 9223372036854775807,
        })
        .unwrap();

        let s = to_string_canonical(&c, CanonicalizationOptions::relaxed()).unwrap();

        assert_eq!(&s, r#"{"a":9223372036854775807}"#);
    }

    #[test]
    fn test_roundtrip() {
        let c: Canonical<A> = serde_json::from_str(r#"{"a": 2, "b": 3}"#).unwrap();
        assert_eq!(c.as_ref(), &A { a: 2 });

        let s = to_string_canonical(&c, CanonicalizationOptions::strict()).unwrap();

        assert_eq!(&s, r#"{"a":2,"b":3}"#);
    }
}
