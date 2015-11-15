FROM ubuntu:vivid

COPY rel/syrup /root/syrup

CMD /root/syrup/bin/syrup foreground
