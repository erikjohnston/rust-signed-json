mod canonical;
pub mod json;
#[cfg(feature = "signed")]
pub mod signed;

use std::convert::TryInto;

use anyhow::Error;
pub use canonical::Canonical;
#[doc(inline)]
pub use json::{to_string_canonical, to_vec_canonical, ENFORCE_INT_RANGE};
#[doc(inline)]
#[cfg(feature = "signed")]
pub use signed::Signed;

pub use ed25519_dalek::{SigningKey, VerifyingKey};

/// Create a [`SigningKey`] from a serialized [`ed25519_dalek::SecretKey`].
pub fn keypair_from_secret_bytes(bytes: &[u8]) -> Result<SigningKey, Error> {
    let secret = SigningKey::from_bytes(bytes.try_into()?);

    Ok(secret)
}
