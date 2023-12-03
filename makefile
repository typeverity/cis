server_src := server/*.hs lib/*.hs

out/cisserver: $(server_src) docker-image
	docker run --user glenda --rm \
		-v ./dist-newstyle:/app/dist-newstyle \
		-v ~/.cabal:/home/glenda/.cabal \
		-v $(PWD):/app \
		-w /app \
		ghcr.io/typeverity/ghc-base:latest \
		sh -c 'cabal update && cabal build cisserver --enable-executable-static && cp $(cabal list-bin cisserver) out/bootstrap'

.PHONY: docker-image
docker-image:
	docker build -t ghc -f ghc.Dockerfile .

