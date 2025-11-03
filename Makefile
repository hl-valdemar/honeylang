BUILD_DIR = build
BIN_NAME = honey

.PHONY: run debug release clean

run: ${BUILD_DIR}
	odin run src -debug -out:${BUILD_DIR}/${BIN_NAME}

debug: ${BUILD_DIR}
	odin build src  -debug -out:${BUILD_DIR}/${BIN_NAME}

release: ${BUILD_DIR}
	odin build src -o:speed -out:${BUILD_DIR}/${BIN_NAME}

clean:
	rm -rf ${BUILD_DIR}

${BUILD_DIR}:
	mkdir -p ${BUILD_DIR}
