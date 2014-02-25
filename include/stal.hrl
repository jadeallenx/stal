-record(project, {
    github_id,
    name,
    full_name,
    description,
    is_fork = false,
    clone_url,
    ssh_url,
    contents_url,
    homepage,
    created_at,
    pushed_at,
    popularity = 0,
    config = []}).
