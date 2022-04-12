#chamando pacotes a serem usados
library(xlsx)
library(FactoMineR)
library(factorextra)
library(readxl)

# esse comando ajuda a encontrar e a chamar mais facil os arquivos
file.choose
df21 = read_excel(file.choose())

#chamando os dados do computador
dataset19<- read_xlsx("/Users/annem/OneDrive/Área de Trabalho/Analises_PCA/estatistica_19.xlsx", 
                      col_names = TRUE, col_types = NULL)
View(dataset19)

#sumarização dos dados
summary(dataset19)
class(dataset19)
View(dataset19)

#realizando a pca
res19 <- PCA(dataset19[2:22], scale = TRUE)

class(res19)
summary(res19) # summary é um método no R

round(res19$eig,3) # autovalores
round(res19$svd$V,3) #autovetores


# A proporção de variação retida pelos componentes principais (CP) pode ser extraída da seguinte forma
round(res19$eig,2)


# A importância dos CP pode ser visualizada usando o scree plot :

fviz_screeplot(res19, ncp=4, title= 'Contribuição das Variáveis nas dimensões 1 e 2')+ 
  theme_minimal()


#######Componentes principais com variaveis padronizadas######

# A correlação entre uma variável e um CP é chamada de carga (loadings). 
round(res19$var$cor,4)

# autovetores
round(res19$svd$V,3)
# autovalores
round(res19$eig,3)

# A proporção de variação retida pelos componentes principais (CP) pode ser extraída da seguinte forma
res19$eig



#Mapa Fatorial
#Quando um subespaço projetivo bidimensional 
#determinado por duas direções principais escolhidas (CP), 
#sua imagem geométrica plana com os pontos projetados 
#e o círculo de correlações é denominada MAPA FATORIAL


# Quanto mais próxima uma variável for do círculo de correlações, melhor sua representação no mapa fatorial (e mais importante é a variável para a interpretação desses componentes)
# As variáveis próximas ao centro do gráfico são menos importantes para os primeiros componentes.
# No gráfico abaixo os componentes são coloridas de acordo com os valores do coseno quadrado:

fviz_pca_var(res19, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal() + labs(title = "Gráfico PCA para 2019",
                                                                           x = "pca1",
                                                                           y= "pca2")


# Cos2: é uma medida que indica a qualidade da representação para variáveis no mapa fatorial
round(res19$var$cos2,2)

# A contribuição das variáveis pode ser extraída da seguinte forma:
round(res19$var$contrib,2)

# veja que a soma é igual a 100%
sum (res19$var$contrib[,1])  


# Quanto maior o valor da contribuição, mais a variável contribui para o componente.
# As variáveis mais importantes associadas a um determinado PC podem ser visualizadas, usando a função fviz_contrib () [factoextra package], da seguinte forma:
# Contribuições de variáveis no PC1
fviz_contrib(res19, choice = "var", axes = 1)+ theme_minimal()+ labs(title = "Contribuição das variaveis na dimensão 1")

# Contribuições de variáveis no PC2
fviz_contrib(res19, choice = "var", axes = 2)+ theme_minimal() + labs(title = "Contribuição das variaveis na dimensão 2")

# Contribuição total nos PC1 e PC2
fviz_contrib(res19, choice = "var", axes = 1:2)+ theme_minimal() + labs(title = "Contribuição das variaveis na dimensão 1- 2")

# Controle as cores das variáveis usando suas contribuições
# a cor representa a contribuição conjunta dim1-dim2
fviz_pca_var(res19, col.var="contrib")+ theme_minimal() + labs(title = "Variaveis principais componentes 2019")



