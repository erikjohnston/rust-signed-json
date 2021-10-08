use serde::de::{Deserialize, DeserializeOwned, Deserializer};
use serde::ser::{Serialize, Serializer};
use serde_json::error::Error;
use std::convert::AsRef;
use std::ops::Deref;

use crate::json::canonicalize_deserializer;
use crate::to_string_canonical;

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
pub struct Canonical<V> {
    value: V,
    canonical_json: String,
}

impl<V> Canonical<V>
where
    V: Serialize,
{
    /// Wrap an existing value and use its serialization as the canonical JSON.
    pub fn wrap(value: V) -> Result<Canonical<V>, Error> {
        let val: serde_json::Value = serde_json::to_value(&value)?;

        let canonical_json = to_string_canonical(&val)?;

        Ok(Canonical {
            value,
            canonical_json,
        })
    }
}

impl<V> Canonical<V> {
    /// Get the stored canonical JSON representation of the wrapped value.
    pub fn get_canonical(&self) -> &str {
        &self.canonical_json
    }
}

impl<V> AsRef<V> for Canonical<V> {
    fn as_ref(&self) -> &V {
        &self.value
    }
}

impl<V> Deref for Canonical<V> {
    type Target = V;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<V> Serialize for Canonical<V> {
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

impl<'de, V> Deserialize<'de> for Canonical<V>
where
    V: DeserializeOwned,
{
    fn deserialize<D>(deserializer: D) -> Result<Canonical<V>, D::Error>
    where
        D: Deserializer<'de>,
    {
        let canonical_json =
            canonicalize_deserializer(deserializer).map_err(serde::de::Error::custom)?;

        let value = serde_json::from_str(&canonical_json).map_err(serde::de::Error::custom)?;

        Ok(Canonical {
            value,
            canonical_json,
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde::{Deserialize, Serialize};

    #[derive(Debug, Serialize, Deserialize, PartialEq)]
    struct A {
        a: u32,
    }

    #[test]
    fn test_deserialize() {
        let c: Canonical<A> = serde_json::from_str(r#"{"a": 2}"#).unwrap();

        assert_eq!(c.as_ref(), &A { a: 2 });
    }

    #[test]
    fn test_serialize() {
        let c = Canonical::wrap(A { a: 2 }).unwrap();

        let s = to_string_canonical(&c).unwrap();

        assert_eq!(&s, r#"{"a":2}"#);
    }

    #[test]
    fn test_roundtrip() {
        let c: Canonical<A> = serde_json::from_str(r#"{"a": 2, "b": 3}"#).unwrap();
        assert_eq!(c.as_ref(), &A { a: 2 });

        let s = to_string_canonical(&c).unwrap();

        assert_eq!(&s, r#"{"a":2,"b":3}"#);
    }
}
