use serde::{Deserialize, Serialize};
use loro::{LoroDoc, LoroValue, LoroMap};

#[derive(Serialize, Deserialize, Debug, Clone)]
#[serde(tag = "type")]
pub enum ProofAST {
    Var { name: String },
    App { func: Box<ProofAST>, arg: Box<ProofAST> },
    Abs { var: String, body: Box<ProofAST> },
    Dup { var: String, dp1: String, dp2: String, body: Box<ProofAST> },
    Fix { var: String, body: Box<ProofAST> },
    Cons { tag: u32, args: Vec<ProofAST> },
    Symbol { name: String },
}

pub fn export_proof(doc: &LoroDoc) -> String {
    let json_value = doc.get_map("proof").get_value().to_json_value();
    serde_json::to_string_pretty(&json_value).unwrap()
}

pub fn import_proof(doc: &mut LoroDoc, json: &str) {
    let value: serde_json::Value = serde_json::from_str(json).unwrap();
    let map = doc.get_map("proof");
    insert_json_into_loro(&map, value);
}

fn insert_json_into_loro(loro_map: &LoroMap, value: serde_json::Value) {
    match value {
        serde_json::Value::Object(map) => {
            for (k, v) in map {
                match v {
                    serde_json::Value::String(s) => { loro_map.insert(&k, s).unwrap(); },
                    serde_json::Value::Number(n) => { loro_map.insert(&k, n.as_f64().unwrap()).unwrap(); },
                    serde_json::Value::Bool(b) => { loro_map.insert(&k, b).unwrap(); },
                    serde_json::Value::Array(arr) => {
                        let loro_list = loro_map.insert_container(&k, loro::ContainerType::List).unwrap().into_list().unwrap();
                        insert_json_array_into_loro_list(&loro_list, arr);
                    },
                    serde_json::Value::Object(obj) => {
                        let inner_map = loro_map.insert_container(&k, loro::ContainerType::Map).unwrap().into_map().unwrap();
                        insert_json_into_loro(&inner_map, serde_json::Value::Object(obj));
                    },
                    serde_json::Value::Null => { /* Handle null if necessary */ },
                }
            }
        },
        _ => panic!("Expected JSON object at root"),
    }
}

fn insert_json_array_into_loro_list(loro_list: &loro::LoroList, arr: Vec<serde_json::Value>) {
    for v in arr {
        match v {
            serde_json::Value::String(s) => { loro_list.push(s).unwrap(); },
            serde_json::Value::Number(n) => { loro_list.push(n.as_f64().unwrap()).unwrap(); },
            serde_json::Value::Bool(b) => { loro_list.push(b).unwrap(); },
            serde_json::Value::Array(inner_arr) => {
                let inner_list = loro_list.insert_container(loro_list.len(), loro::ContainerType::List).unwrap().into_list().unwrap();
                insert_json_array_into_loro_list(&inner_list, inner_arr);
            },
            serde_json::Value::Object(obj) => {
                let inner_map = loro_list.insert_container(loro_list.len(), loro::ContainerType::Map).unwrap().into_map().unwrap();
                insert_json_into_loro(&inner_map, serde_json::Value::Object(obj));
            },
            serde_json::Value::Null => { /* Handle null */ },
        }
    }
}
