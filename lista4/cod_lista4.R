
#########################
### Geração dos dados ###
#########################

set.seed(2345) # semente


#########################
### Avaliação via TRI ###
#########################


# ---- 1ª parte - pegar estimativas reais e gerar respostas

library(irtoys)

library(mirt)


### Valores estimados dos parametros no modelo de resposta gradual
### para o questionario de HIPERATIVIDADE em 
### Costa, M.C.M (2014) - Aplicando a Teoria de Resposta ao Item a 
### dados psicométricos, UFRJ.


# estimativas dos parametros a partir das respostas reais:
a.par <- c(2.258,2.019,2.250,2.072,2.654,2.873,3.475,3.465,2.949)
b1.par <- c(-0.347,-0.144,0.615,0.317,0.269,-0.321,-0.289,-0.489,-0.547)
b2.par <- c(0.527,0.708,1.342,1.437,1.138,0.444,0.592,0.303,0.311)
b3.par <- c(1.515,1.560,1.959,1.986,1.940,1.452,1.622,1.210,1.409)
n.itens <- length(a.par)

nr <- 1000 # numero de respondentes

### geracao das proficiencias
theta <- rnorm(nr,0,1)
resp <- matrix(0,nr,n.itens)

### geracao das respostas

# matriz para guardar as probabilidades
mat.prob <- matrix(0,n.itens,nr) 

# # lista para guardar as matrizes
# lista_mat.prob <- list() # nao precisaremos agora

for (j in 1:nr) {
  # calculo do exponencial para cada b_k dentro da formula da prob P_ik(theta_j)
  mat.prob <- cbind(rep(0,n.itens),
                    exp(-a.par*(theta[j]-b1.par)),
                    exp(-a.par*(theta[j]-b2.par)),
                    exp(-a.par*(theta[j]-b3.par)))
  
  # calculo da probabilidade
  mat.prob <- 1/(1+mat.prob)
  
  # para calcular o que falta para dar 1
  mat.prob <- cbind(-t(apply(mat.prob,1,diff)),mat.prob[,4])
  
  # nao estah guadando a probabilidade
  # lista_mat.prob[[j]] <- mat.prob
  
  for (i in 1:n.itens)
    resp[j,i] <- sample(4,1,replace=F,mat.prob[i,])
}

# lista_mat.prob

# teste para um unico b_ik
# grafico

# mat.prob_geral <- Reduce("rbind", lista_mat.prob)
# 
# prob_b1 <- mat.prob_geral[,1]
# 
# data_long <- as.data.frame(cbind(rep(1:9, 9000/9), rep(theta, each=9), prob_b1))
# 
# str(data_long)
# 
# colnames(data_long) <- c("item", "theta", "value")
# 
# mypalette <- c( ' #e6194b ' , "#3cb44b", "#ffe119")
# 
# p1 <- data_long %>%
#   ggplot(aes(x=theta, y=value, colour=as.factor(as.character(item)))) +
#   geom_line()+
#   xlab( ' Theta ' )+
#   ylab("Prob")+
#   ggtitle( ' Curva Caracterı́stica de cada Item (CCI) ' )+
#   theme(plot.title = element_text(hjust=0.5))
# p1

# ok funcionando



# --- 2ª parte - utilizar as respostas geradas (com os thetas simulados) para estimar novos parametros

### ajuste do modelo de resposta gradual via "mirt" utilizando as respostas simuladas
# write(t(resp),file="dados.mrg.txt",ncol=9)
# resp <- read.table(file="dados.mrg.txt")

# novo
resp <- as.data.frame(matrix(t(resp), ncol=9))

mrg <- mirt(resp,1,itemtype=c('graded'))

prof.est <- fscores(mrg, full.scores=TRUE)
par.est <- coef(mrg,IRTpars=TRUE)

# plot(theta,prof.est)
# abline(0,1)
# 
# resp[prof.est==min(prof.est),]
# 
# resp[prof.est==max(prof.est),]

### Estimativas reais dos parametros dos itens utilizadas para simular os dados
#         a      b1    b2    b3
#Item 1 2.258 -0.347 0.527 1.515
#Item 2 2.019 -0.144 0.708 1.560
#Item 3 2.250  0.615 1.342 1.959
#Item 4 2.072  0.317 1.437 1.986
#Item 5 2.654  0.269 1.138 1.940
#Item 6 2.873 -0.321 0.444 1.452
#Item 7 3.475 -0.289 0.592 1.622
#Item 8 3.465 -0.489 0.303 1.210
#Item 9 2.949 -0.547 0.311 1.409


# --- 3ª parte - preparar os dados para gerar as CCI's

###  objetivo: grafico com as CCI ' s de cada item POR CATEGORIA
# usar facett_wrap para itens

# retirando ultimo item da lista par.est
par.est2 <- par.est[-length(par.est)]

# passando de lista para matriz
par.est_n <- Reduce('rbind', par.est2)


a.par_n <- par.est_n[,1]
b1.par_n <- par.est_n[,2]
b2.par_n <- par.est_n[,3]
b3.par_n <- par.est_n[,4]


### geracao das proficiencias
# theta <- rnorm(nr,0,1)

# criando um range gde de thetas
theta <- seq(-20,20, 0.01)

### geracao das respostas

# matriz para guardar a sprobabilidades
mat.prob_n <- matrix(0,n.itens,length(theta)) #teremos 1 matriz 9x4 para cada respondente (theta_j)

# # lista para guardar as matrizes
lista_mat.prob_n <- list() # nao precisaremos agora

for (j in 1:length(theta)) {
  # calculo do exponencial para cada b_k dentro da formula da prob P_ik(theta_j)
  mat.prob_n <- cbind(rep(0,n.itens),
                    exp(-a.par_n*(theta[j]-b1.par_n)),
                    exp(-a.par_n*(theta[j]-b2.par_n)),
                    exp(-a.par_n*(theta[j]-b3.par_n)))
  
  # calculo da probabilidade
  mat.prob_n <- 1/(1+mat.prob_n)
  
  # para calcular o que falta para dar 1
  mat.prob_n <- cbind(-t(apply(mat.prob_n,1,diff)),mat.prob_n[,4])
  
  # nao estah guadando a probabilidade
  lista_mat.prob_n[[j]] <- mat.prob_n # nao eh necessario - depois so pegaremos um range de theta
  
}

lista_mat.prob_n


mat.prob_geral_n <- Reduce("rbind", lista_mat.prob_n)

data_long_n <- as.data.frame(cbind(rep(1:9, nrow(mat.prob_geral_n)/9), rep(theta, each=9), mat.prob_geral_n))

colnames(data_long_n) <- c("item", "theta", "b1", "b2", "b3", "b4")

# se nao der certo, fazer por apply no ggplot

data_long_n2 <- data_long_n %>%
  melt(id = 1:2)

mypalette <- c( '#e6194b' , "#3cb44b", "#ffe119", '#aa6e28')

p1 <- data_long_n2 %>%
  ggplot(aes(x=theta, y=value, colour=as.factor(as.character(variable)))) +
  # colour precisa ser passada dentro do aesthetics para funcionar!
  # sol: https://stackoverflow.com/questions/3777174/
  #plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
  geom_line()+
  xlab( ' Theta ' )+
  ylab("Prob")+
  facet_wrap(~as.factor(item))+
  scale_color_discrete(name = "Legenda")+
  ggtitle( 'Probabilidade de Responder Exatamente uma Categoria k' )+
  theme(plot.title = element_text(hjust=0.5))
# scale_color_discrete("Item", breaks=as.character(1:14))+
# para acertar ordem dos fatores na legenda
# pq estamos usando scale_color_discrete
# scale_colour_manual("Item", breaks=as.character(1:14), values=mypalette)
p1

# ok!!!!!!!





# ---------------- probabilidades P+


### geracao das respostas

# matriz para guardar a sprobabilidades
mat.prob_n <- matrix(0,n.itens,length(theta)) #teremos 1 matriz 9x4 para cada respondente (theta_j)

mat.prob_n <- matrix()

# # lista para guardar as matrizes
lista_mat.prob_n <- list() # nao precisaremos agora

for (j in 1:length(theta)) {
  # calculo das proobabilidades para cada b_k dentro da formula da prob P_ik(theta_j)
  prob_b1 <- 1/(1+exp(-a.par_n*(theta[j]-b1.par_n)))
  prob_b2 <- 1/(1+exp(-a.par_n*(theta[j]-b2.par_n)))
  prob_b3 <- 1/(1+exp(-a.par_n*(theta[j]-b3.par_n)))
  prob_b4 <- rep(1, n.itens) #b_0
  # exp(-a.par_n*(theta[j]-b2.par_n)),
  # exp(-a.par_n*(theta[j]-b3.par_n)))
  
  mat.prob_n <- cbind(prob_b1, prob_b2, prob_b3, prob_b4)
  
  # nao estah guadando a probabilidade
  lista_mat.prob_n[[j]] <- mat.prob_n # nao eh necessario - depois so pegaremos um range de theta
  
}

lista_mat.prob_n


mat.prob_geral_n <- Reduce("rbind", lista_mat.prob_n)

data_long_n <- as.data.frame(cbind(rep(1:9, nrow(mat.prob_geral_n)/9), rep(theta, each=9), mat.prob_geral_n))

colnames(data_long_n) <- c("item", "theta", "b1", "b2", "b3", "b4")

# se nao der certo, fazer por apply no ggplot

data_long_n2 <- data_long_n %>%
  melt(id = 1:2)

mypalette <- c( '#e6194b' , "#3cb44b", "#ffe119", '#aa6e28')

p2 <- data_long_n2 %>%
  ggplot(aes(x=theta, y=value, colour=as.factor(as.character(variable)))) +
  # colour precisa ser passada dentro do aesthetics para funcionar!
  # sol: https://stackoverflow.com/questions/3777174/
  #plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph
  geom_line()+
  xlab( ' Theta ' )+
  ylab("Prob")+
  facet_wrap(~as.factor(item))+
  scale_color_discrete(name = "Legenda")+
  ggtitle( 'Curvas Características de cada Item para cada Categoria k' )+
  theme(plot.title = element_text(hjust=0.5))
# scale_color_discrete("Item", breaks=as.character(1:14))+
# para acertar ordem dos fatores na legenda
# pq estamos usando scale_color_discrete
# scale_colour_manual("Item", breaks=as.character(1:14), values=mypalette)
p2

# ok!!!!!!!