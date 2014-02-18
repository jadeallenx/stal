stal
====
Prototype of an Erlang project index

Build
-----
    git clone https://github.com/mrallen1/stal.git
    cd stal
    rebar get-deps
    rebar compile

Start
-----
    erl -pa ebin -pa deps/*/ebin -- -s stal

Current status
--------------
1. Queries github for Erlang repositories ranked by stars, descending order (top 1000 on the board)
2. Translates the JSON response into Erlang terms
3. Tranforms the essential* parts into Erlang records
4. Loads records into an ETS table

**Essential parts** are:
* github_id (this is the ETS key)
* full_name (owner/project)
* description
* is_fork
* clone_url
* ssh_url
* contents_url (to be used to fetch the project definition)
* created_at (in Erlang calendar format)
* pushed_at (in Erlang calendar format)
* popularity (sum of forks + stars)

### Example ###

    2> stal:github_repo_search().
    [ .. Spammy output omitted .. ]
    3> rr("include/stal.hrl").
    [project]
    3> ets:lookup(github_projects, 1460612).
    [#project{github_id = 1460612,
          full_name = <<"extend/cowboy">>,
          description = <<"Small, fast, modular HTTP server written in Erlang.">>,
          is_fork = false,
          clone_url = <<"https://github.com/extend/cowboy.git">>,
          ssh_url = <<"git@github.com:extend/cowboy.git">>,
          contents_url = <<"https://api.github.com/repos/extend/cowboy/contents/{+path}">>,
          homepage = <<"http://ninenines.eu">>,
          created_at = {{2011,3,9},{19,55,52}},
          pushed_at = {{2014,2,6},{19,0,35}},
          popularity = 1690}]

Next steps
----------
Immediate next steps would be:
* Refactoring to put ets and data fetches into a gen_server

* Serializing ETS table to disk periodically and loading ETS from disk if
  available and not too old

Next steps after that:
* Parsing the project definition file (if it exists) (should be straightforward
  to get this using the contents_url in the project record) - augmenting record
  with information

* Tweaking popular projects which the community has deemed "inactive" or
  "superseded by fork X" so they inherit the popularity of the parent project
  but the active/maintained project shows up in the proper spot

Finally:
* Decide what to do with this information? (Autogen rebar.conf? Autogen a
  makefile, autogen both?)

* Publish index

### Major Problem ###
Github's search API only returns top 1000 results but there are 8473 repos
(total count of language:erlang results) so we are only seeing the 1000 most
popular Erlang projects where popularity is judged by the number of stars a
project has - I don't see any easy way to get the next thousand results from
the search API

I don't know how to get around this; crawling the entire site would be
very frustrating and slow and probably error prone, especially given that
github already knows what the 8000+ repos we care about are.
