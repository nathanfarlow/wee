FROM ocaml/opam:ubuntu
RUN sudo apt-get update && sudo apt-get install -y ruby
RUN git clone --depth=1 https://github.com/shinh/elvm.git
RUN cd elvm && make
COPY src src
RUN sudo chown -R opam:opam src
WORKDIR /home/opam/src
RUN opam update && opam install . --deps-only --verbose
RUN opam exec -- dune build -p wee
RUN sudo cp _build/default/bin/native/main.exe /usr/local/bin/wee