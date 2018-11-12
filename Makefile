all: backend frontend

backend:
	stack install

frontend:
	yarn install
