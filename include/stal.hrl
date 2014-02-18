-define(ACCEPT_HDR, {"Accept", "appliCation/vnd.github.preview"}).
-define(USER_AGENT, {"User-Agent", "stal/Erlang 0.01"}).
-define(GITHUB_REPO_SEARCH_URL, "https://api.github.com/search/repositories?q=language:erlang&sort=stars&order=desc&per_page=100&page=").
-define(HTTPOPTIONS, [{timeout, 5000}, {connect_timeout, 2000}]).
-define(OPTIONS, [{body_format, binary}]).
-define(TABLE_NAME, github_projects).

-record(project, {
    github_id,
    full_name,
    description,
    is_fork = false,
    clone_url,
    ssh_url,
    contents_url,
    homepage,
    created_at,
    pushed_at,
    popularity = 0}).
