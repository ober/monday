.PHONY: test tests
docker:
	docker build --rm=true -t monday .
	docker tag monday jaimef/monday

push:
	docker push jaimef/monday

tests: test

test:
	@./test/test.ss
