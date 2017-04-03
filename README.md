redrum
=====

An escript for chaning deps in rebar.config. Rebar Deps Re-Mapper,
RDRM, with some extra vowels, for fun.

Imagine you have a whole organisations repos checked out, and you push
them all to a new re-organisation. Imagine that you want to build
them, as before, but now all the deps like
`git@github.com/old-org/repoX.git` should be
`https://mygitlab/new-org/repoX.git`. Although some ad hoc shell
scripting will get you there, `rebar.config` can be consulted and
manipulated in erlang.

Given a config that is a set of mappings `redrum` will re-map your
rebar deps.

Build
-----

    $ rebar3 escriptize

Run
---

    $ _build/default/bin/redrum
