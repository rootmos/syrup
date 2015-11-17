FROM unbalancedparentheses/erlang:17.4

WORKDIR /root
COPY rebar.config .

RUN mkdir src
COPY src src

RUN mkdir rel
COPY rel/reltool.config rel
RUN mkdir rel/files
COPY rel/files rel/files

RUN rebar get-deps compile generate

CMD rel/syrup/bin/syrup foreground
