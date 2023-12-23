options(warn = -1)

source("lib/utils.R")
source("lib/json.R")

json = read_stdin_all()
xs = json_parse(json)
json_print(xs)
