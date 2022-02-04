load("~/Documents/abj/jurimetriaTCU/dados_gilson.RData")

# lista <- purrr::map(ls(), ~object.size(eval(parse(text = .x))))
# nums <- as.numeric(lista)
# ls()[order(nums)]

rm(t)
rm(t3)
rm(s3d)
rm(t2t1)
rm(glm.fit)

brutos <- c(
  "tcu_total_2011","tcu_total_2012","tcu_total_2013","tcu_total_2014",
  "tcu_total_2015","tcu_total_2016","tcu_total_2017",
  "tcu_total_2018","tcu_total_2019","tcu_2020"
)

vars_considerar <- setdiff(ls(), brutos)

for (v in vars_considerar) {
  tmp <- eval(parse(text = v))
  if (is.list(tmp)) {
    tmp[["Sumário"]] <- NULL
    tmp[["Interessado / Responsável / Recorrente"]] <- NULL
    tmp[["Endereço do Arquivo"]] <- NULL
    tmp[["Assunto"]] <- NULL
    assign(v, tmp, envir = .GlobalEnv)
  }
}

lista <- purrr::map(ls(), ~object.size(eval(parse(text = .x))))
nums <- as.numeric(lista)
ls()[order(nums)]


save.image("dados.RData", compress = "xz")
