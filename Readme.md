# retrofit-rs

[Retrofit](https://square.github.io/retrofit/) is a type-safe HTTP client for Android and Java. This is a Rust port of Retrofit.

## Why?

Assemble a http request with header, path, query, body, etc. is a boring work. Retrofit can help you to do this work.

## Usage

```rust
#[derive(Debug, serde::Serialize, serde::Deserialize)]
struct Repo {
    pub name: String,
    pub full_name: String,
    pub fork: bool,
    pub url: String,
}

retrofit_derive::retrofit!(
    trait Client {
        #[get("/users/{user}/repos")]
        #[header("User-Agent", "retrofit-rs")]
        async fn list_repos(&self, #[path("user")]  user: String) -> anyhow::Result<Vec<Repo>>;
    }
);

#[tokio::main]
async fn main() -> anyhow::Result<()> {

    let client = ClientBuilder::new()
        .base_url("https://api.github.com".to_string())
        //.client(client)
        .build();

    let repo = client.list_repos("hxzhao527".to_string()).await?;

    println!("{:?}", repo.first());
    Ok(())
}
```

# How it works

__use proc-macro to generate code__

Now, it depends on reqwest to send http request. Maybe it will support other http client in the future.