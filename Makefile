.PHONY: deploy
deploy: deploy-web deploy-backend

.PHONY: deploy-web
deploy-web: build-frontend clear-deploy-dir
	tar -caf ${DEPLOY_DIR}/londonspex-frontend.tar.gz web/build
	cd ${DEPLOY_DIR} && tar -xf londonspex-frontend.tar.gz

clear-deploy-dir: ${DEPLOY_DIR}
	rm -r ${DEPLOY_DIR}/*

${DEPLOY_DIR}:
	mkdir -p ${DEPLOY_DIR}

.PHONY: backend-bin
backend-bin:
	stack install

build-frontend:
	cd web/ && yarn build
