// refactor this Rust function to return a random UUID (add use statements as needed)
fn foo() {
}


```rust
use uuid::Uuid;

fn foo() -> Uuid {
    Uuid::new_v4()
}
```

Make sure to add `uuid = "1.0"` to your `Cargo.toml` dependencies for this code to work.
