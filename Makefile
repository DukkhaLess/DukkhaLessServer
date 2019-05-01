stack_yaml = STACK_YAML="stack.yaml"
stack = ${stack_yaml} stack
db_container_name = DukkhalessDB
postgres_password = secret
db_script_name = db_up_script.psql


build:
	${stack} build

run:
	$(MAKE) db_up
	$(MAKE) build
	${stack} exec dukkhaless-app Development

db_up:
	docker-compose up -d
	export PGPASSWORD=${postgres_password}; \
	psql -h localhost -p 5432 -U postgres < ${db_script_name}

stop:
	docker stop ${db_container_name}
