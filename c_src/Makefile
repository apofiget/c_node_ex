
CC=gcc
CMD_DIR= $(dir$(lastword $(MAKEFILE_LIST)))../priv
CMD_PATH= $(CMD_DIR)/erl_c_node

all: dirs compile

dirs:
	-@mkdir -p $(CMD_DIR)

compile:
	$(CC) -o $(CMD_PATH)
