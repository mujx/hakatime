db:
	@docker-compose down
	@docker-compose up -d

db-repl:
	@pgcli -h localhost -p 5432 -u test -W test -d test

clean:
	@git clean -xdf

shell:
	@nix-shell

ui:
	cd ./dashboard && yarn install && yarn run prod && yarn run lint

gen-nix:
	cabal2nix --no-haddock --no-check . > default.nix

build-img:
	docker build -t hakatime:latest .

arm:
	docker buildx build --platform=linux/arm,linux/arm64 -f Dockerfile.arm . -t mujx/hakatime:latest-arm --push

arm-deps:
	docker buildx build --platform=linux/arm,linux/arm64 -f Dockerfile.arm-deps . -t mujx/hakatime-arm-deps:latest --push
