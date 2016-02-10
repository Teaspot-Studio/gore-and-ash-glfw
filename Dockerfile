FROM phadej/ghc:7.10.2 

RUN apt-get update
RUN apt-get install -y libglfw3-dev

ADD mkdocs /opt/project/
ADD LICENSE /opt/project/
ADD Setup.hs /opt/project/
ADD README.md /opt/project/
ADD gore-and-ash-glfw.cabal /opt/project/
ADD stack.yaml /opt/project/
ADD src /opt/project/src

WORKDIR /opt/project

ENTRYPOINT ["./mkdocs", "gore-and-ash-glfw", "1.1.0.0", "NCrashed"]