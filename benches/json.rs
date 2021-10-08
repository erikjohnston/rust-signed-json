#![feature(test)]

extern crate test;

use serde::{Deserialize, Serialize};
use serde_json::{json, to_vec, Value};
use test::Bencher;

use signed_json::{json::to_vec_canonical, Canonical, Signed};

#[bench]
fn bench_empty_canonical(b: &mut Bencher) {
    let value = json!({});
    b.iter(|| to_vec_canonical(&value));
}

#[bench]
fn bench_empty_serde(b: &mut Bencher) {
    let value = json!({});
    b.iter(|| to_vec(&value));
}

#[bench]
fn bench_simple_canonical(b: &mut Bencher) {
    let value = json!({
        "abcd": "some string",
        "map": {
            "a": "field",
        }
    });
    b.iter(|| to_vec_canonical(&value));
}

#[bench]
fn bench_simple_serde(b: &mut Bencher) {
    let value = json!({
        "abcd": "some string",
        "map": {
            "a": "field",
        }
    });
    b.iter(|| to_vec(&value));
}

#[bench]
fn parse_simple_serde(b: &mut Bencher) {
    #[derive(Deserialize)]
    pub struct Test {
        pub a: u8,
        pub b: Value,
    }

    let json = r#"{
        "a": 1,
        "b": {
            "c": "foobar",
            "d": 1
        }
    }"#;

    b.iter(|| {
        let test: Test = serde_json::from_str(json).unwrap();
        test
    });
}

#[bench]
fn parse_simple_canonical(b: &mut Bencher) {
    #[derive(Deserialize)]
    pub struct Test {
        pub a: u8,
        pub b: Value,
    }

    let json = r#"{
        "a": 1,
        "b": {
            "c": "foobar",
            "d": 1
        }
    }"#;

    b.iter(|| {
        let test: Canonical<Test> = serde_json::from_str(json).unwrap();
        test
    });
}

#[bench]
fn parse_simple_signed(b: &mut Bencher) {
    #[derive(Deserialize)]
    pub struct Test {
        pub a: u8,
        pub b: Value,
    }

    let json = r#"{
        "a": 1,
        "b": {
            "c": "foobar",
            "d": 1
        }
    }"#;

    b.iter(|| {
        let test: Signed<Test> = serde_json::from_str(json).unwrap();
        test
    });
}

#[bench]
fn serialize_serde(b: &mut Bencher) {
    #[derive(Deserialize, Serialize)]
    pub struct Test {
        pub a: u8,
        pub b: Value,
    }

    let json = r#"{
        "a": 1,
        "b": {
            "c": "foobar",
            "d": 1
        }
    }"#;

    let test: Test = serde_json::from_str(json).unwrap();

    b.iter(|| serde_json::to_string(&test));
}

#[bench]
fn serialize_canonical(b: &mut Bencher) {
    #[derive(Deserialize)]
    pub struct Test {
        pub a: u8,
        pub b: Value,
    }

    let json = r#"{
        "a": 1,
        "b": {
            "c": "foobar",
            "d": 1
        }
    }"#;

    let test: Canonical<Test> = serde_json::from_str(json).unwrap();

    b.iter(|| serde_json::to_string(&test));
}

#[bench]
fn serialize_signed(b: &mut Bencher) {
    #[derive(Deserialize)]
    pub struct Test {
        pub a: u8,
        pub b: Value,
    }

    let json = r#"{
        "a": 1,
        "b": {
            "c": "foobar",
            "d": 1
        }
    }"#;

    let test: Signed<Test> = serde_json::from_str(json).unwrap();

    b.iter(|| serde_json::to_string(&test));
}
