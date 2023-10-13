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

        #[get("/repos/{owner}/{repo}/forks")]
        #[header("User-Agent", "retrofit-rs")]
        async fn list_forks(&self, #[path("owner")] owner: &str, #[path("repo")] repo: &str) -> anyhow::Result<Vec<Repo>>;
    }
);

#[tokio::main]
async fn main() -> anyhow::Result<()> {

    let client = ClientBuilder::new()
        .base_url("https://api.github.com".to_string())
        //.client(client)
        .build();

    let repo = client.list_repos("hxzhao527".to_string()).await?;
    println!("list_repos: {:?}", repo.first());

    let repo = client.list_forks("rust-lang", "rust").await?;
    println!("list_forks: {:?}", repo.first());

    Ok(())
}