passagem <- 1500
n_pesq <- 3
diaria <- 480
med_dias <- 3
n_eventos <- 12

# Estimar valor de viagens para eventos

estimar_viagens <- function(n_pesq, passagem, diaria, med_dias, n_eventos){
n_pesq * (passagem  + (diaria * med_dias)) * n_eventos
}

# 3 pessoas viajam para 12 eventos no ano
estimar_viagens(3, 1500, 480, 3, 12)

# Estimar valor dos bolsistas

salario_bolsistas <- c(AuxP = 1225,
                       API = 2210,
                       APII = 3360,
                       APIII = 4030,
                       APIV = 5320, 
                       DOU = 6760, 
                       PV = 4940,
                       IPI = 4030,
                       IPII = 6240,
                       PCI = 3120,
                       PCII = 6240,
                       PSEN = 6240)

bolsistas <- c(API = 2, APII = 1)

meses_projeto <- 24

estimar_bolsistas <- function(meses_projeto, bolsistas){
tabela <- merge(bolsistas, salario_bolsistas, by = 0)
tabela$total <- apply(tabela[2:3], 1, prod) * meses_projeto
return(list(tabela, sum(tabela$total)))
}

estimar_bolsistas(24, c(API = 1, APII = 1))

# Estimar custo de 

estimar_funcionarios <- function(n_func, horas_sem, salario_hora, meses_projeto){
    prod(n_func, horas_sem, salario_hora, meses_projeto)
}

sal_med <- mean(c(21920, 24000, 28000))

tpp <- estimar_funcionarios(3, 10, sal_med/40, 24)
tpp

sal_aux <- 7000
aux <- estimar_funcionarios(1, 10, sal_aux/40, 24)

aux + tpp
