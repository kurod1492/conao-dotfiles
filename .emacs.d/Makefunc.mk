#
# Makefunk.mk
#
# version: v1.4
# last update: 2018/11/22
#
# echo with color

ECHO_COLOR   = /bin/echo -n $3; \
               tput setaf $1; \
               /bin/echo $5 "=== " $2 " ==="; \
               /bin/echo -n $4; \
               tput sgr0

ECHO_BLACK   = $(call ECHO_COLOR, 0, $1, $2, $3)
ECHO_RED     = $(call ECHO_COLOR, 1, $1, $2, $3)
ECHO_GREEN   = $(call ECHO_COLOR, 2, $1, $2, $3)
ECHO_YELLOW  = $(call ECHO_COLOR, 3, $1, $2, $3)
ECHO_BLUE    = $(call ECHO_COLOR, 4, $1, $2, $3)
ECHO_MAGENTA = $(call ECHO_COLOR, 5, $1, $2, $3)
ECHO_CYAN    = $(call ECHO_COLOR, 6, $1, $2, $3)
ECHO_WHITE   = $(call ECHO_COLOR, 7, $1, $2, $3)

ECHO_BLACK-   = $(call ECHO_COLOR, 0, $1, $2, $3, -n)
ECHO_RED-     = $(call ECHO_COLOR, 1, $1, $2, $3, -n)
ECHO_GREEN-   = $(call ECHO_COLOR, 2, $1, $2, $3, -n)
ECHO_YELLOW-  = $(call ECHO_COLOR, 3, $1, $2, $3, -n)
ECHO_BLUE-    = $(call ECHO_COLOR, 4, $1, $2, $3, -n)
ECHO_MAGENTA- = $(call ECHO_COLOR, 5, $1, $2, $3, -n)
ECHO_CYAN-    = $(call ECHO_COLOR, 6, $1, $2, $3, -n)
ECHO_WHITE    = $(call ECHO_COLOR, 7, $1, $2, $3, -n)

COLOR_BLACK   = tput setaf 0
COLOR_RED     = tput setaf 1
COLOR_GREEN   = tput setaf 2
COLOR_YELLOW  = tput setaf 3
COLOR_BLUE    = tput setaf 4
COLOR_MAGENTA = tput setaf 5
COLOR_CYAN    = tput setaf 6
COLOR_WHITE   = tput setaf 7
COLOR_DEFAULT = tput sgr0

colortest:
	@$(call ECHO_BLACK, black)
	@$(call ECHO_RED, red)
	@$(call ECHO_GREEN, green)
	@$(call ECHO_YELLOW, yellow)
	@$(call ECHO_BLUE, blue)
	@$(call ECHO_MAGENTA, magenta)
	@$(call ECHO_CYAN, cyan)
	@$(call ECHO_WHOTE, white)
