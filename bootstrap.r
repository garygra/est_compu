#!/usr/bin/Rscript
library(datasets)
library(tidyverse)
library(ggplot2)
library(formattable)
library(bootstrap)

beta <- 1
set.seed(261285)
muestra <- rexp(20, 1/beta)

mediaBoot <- function(x){ 
  muestra_boot <- sample(x, size = length(x), replace = TRUE) # Una remuestra
  mean(muestra_boot) # Replicacion bootstrap de theta_gorro
}
# thetas_boot <- rerun(2000, mediaBoot(muestra)) %>% flatten_dbl()
# mean(thetas_boot)
# sd(thetas_boot)


se <- function(x) sqrt(sum((x - mean(x)) ^ 2)) / length(x)
se(muestra)

print("Normal IC")
skewness <- function(x){
    n <- length(x)
    1 / n * sum((x - mean(x)) ^ 3) / sd(x) ^ 3 
}
theta_hat <-skewness(muestra)
cat("theta_hat: ", theta_hat, "\n")

skewness_boot <- function(x_boot){
  x_boot <- sample(x, length(x), replace = TRUE)
  skewness(x_boot)
}
thetas_boot2 <- rerun(2000, mediaBoot(muestra)) %>% flatten_dbl()
mean(thetas_boot2)
sd(thetas_boot2)
li_normal <- round(theta_hat - 1.96 * sd(thetas_boot2), 2)
ls_normal <- round(theta_hat + 1.96 * sd(thetas_boot2), 2)
cat("intervalo normal: (", li_normal, ", ", ls_normal, ")\n")

df_thetas_boot2 <- thetas_boot2 %>% as_data_frame()
ggplot(df_thetas_boot2) +
    geom_histogram(aes(x = value, y = ..density..), binwidth = 0.05, fill = "gray40") + 
    geom_vline(xintercept = c(li_normal, ls_normal, theta_hat), color = c("black", "black", "red"), alpha = 0.5)

ggplot(df_thetas_boot2) + 
	geom_abline(color = "red", alpha = 0.5) + 
	stat_qq(aes(sample = thetas_boot2), dparams = list(mean = mean(thetas_boot2), sd = sd(thetas_boot2))) 

comma(q_kurt <- quantile(thetas_boot2, probs = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975)))
comma(qnorm(p = c(0.025, 0.05, 0.1, 0.9, 0.95, 0.975), mean = theta_hat, sd = sd(thetas_boot2)))


cat("Percentiles:\n")
ggplot(arrange(df_thetas_boot2, thetas_boot2)) + 
	stat_ecdf(aes(x = thetas_boot2)) + 
	geom_segment(data = data_frame(x = c(-Inf, -Inf, q_kurt[c(1, 6)]), 
		xend = q_kurt[c(1, 6, 1, 6)], y = c(0.025, 0.975, 0, 0), 
		yend = c(0.025, 0.975, 0.025, 0.975)), aes(x = x, xend = xend, y = y, 
		yend = yend), color = "red", size = 0.4, alpha = 0.5) + 
	labs(x = "Cuantiles muestrales", y = "ecdf")

ls_per <- round(quantile(thetas_boot2, prob = 0.975), 2)
li_per <- round(quantile(thetas_boot2, prob = 0.025), 2)
cat("intervalo percentil: (", li_per, ", ", ls_per, ")\n")


cat("\nBC_alpha\n")

# bcanon(x = muestra, nboot = 2000, theta = se)

b_var <- rerun(2000, se(sample(muestra, size = length(muestra), replace = TRUE))) %>% flatten_dbl()
qplot(b_var) + geom_histogram(bins = 10)

ggplot(data_frame(b_var)) +
    geom_abline(color = "red", alpha = 0.5) +
    stat_qq(aes(sample = b_var), 
        dparams = list(mean = mean(b_var), sd = sd(b_var))) +
  geom_abline()

ls_bc <- round(quantile(b_var, prob = 0.95), 2)
li_bc <- round(quantile(b_var, prob = 0.05), 2)
cat("intervalo BC_alpha: (", li_bc, ", ", ls_bc, ")\n")

cat("\nFinished!\n")
cat("Los intervalos son:\n")

cat("intervalo normal:\t(", li_normal, ", ", ls_normal, ")\n")
cat("intervalo percentil:\t(", li_per, ", ", ls_per, ")\n")
cat("intervalo BC_alpha:\t(", li_bc, ", ", ls_bc, ")\n")




proc_examen <- function()
{

	mediaBoot <- function(x)
	{ 
		muestra_boot <- sample(x, size = length(x), replace = TRUE) 
		mean(muestra_boot) # Replicacion bootstrap de theta_gorro
	}
	se <- function(x) sum((x - mean(x)) ^ 2) / length(x)
	skewness <- function(x)
	{
    	n <- length(x)
    	1 / n * sum((x - mean(x)) ^ 3) / sd(x) ^ 3 
	}
	skewness_boot <- function(x_boot)
	{
		x_boot <- sample(x, length(x), replace = TRUE)
		skewness(x_boot)
	}
	muestra <- rexp(20, 1/beta)
	thetas_boot <- rerun(2000, mediaBoot(muestra)) %>% flatten_dbl()
	theta_hat <-skewness(muestra)

	li_normal <- round(theta_hat - 1.96 * sd(thetas_boot), 2)
	ls_normal <- round(theta_hat + 1.96 * sd(thetas_boot), 2)

	ls_per <- round(quantile(thetas_boot, prob = 0.975), 2)
	li_per <- round(quantile(thetas_boot, prob = 0.025), 2)

	b_var <- rerun(2000, 
				se(sample(muestra, size = length(muestra), 
					replace = TRUE))) %>% flatten_dbl()
	ls_bc <- round(quantile(b_var, prob = 0.95), 2)
	li_bc <- round(quantile(b_var, prob = 0.05), 2)

	return(c(li_normal, ls_normal, li_per, ls_per, li_bc, ls_bc))
}

get_df_from_res <- function(arr)
{	
	df <- tribble(
  		~prueba, ~li_normal, ~ls_normal, ~li_per, ~ls_per, ~li_bc, ~ls_bc
	)
	cont <- 1
	for (i in 1:length(arr))
	{
		# cat("i: ", i, "\n")
	  switch(((i-1) %% 6)+1,
	  	li_normal <- arr[i],
	  	ls_normal <- arr[i],
	  	li_per <- arr[i],
	  	ls_per <- arr[i],
	  	{
			# cat("i_5: ", i, "\n")
	  		li_bc <- arr[i]
	  	},
	  	{
			cat("i_6: ", i, "\n")
	  		ls_bc <- arr[i]
	  		df <- add_row(df, prueba = cont, 
	  					li_normal = li_normal,
	  					ls_normal = ls_normal,
	  					li_per = li_per,
	  					ls_per = ls_per,
	  					li_bc = li_bc,
	  					ls_bc = ls_bc
	  			)
	  		cont <- cont + 1
	  	}
	  	)

	}
	return(df)
}

res_aux <- rerun(500, proc_examen()) %>% flatten_dbl()
df_res_aux <- get_df_from_res(res_aux)

norm_izq <- nrow(filter(df_res_aux, li_normal > 1))
norm_der <- nrow(filter(df_res_aux, ls_normal < 1))
per_izq <- nrow(filter(df_res_aux, li_per > 1))
per_der <- nrow(filter(df_res_aux, ls_per < 1))
bc_izq <- nrow(filter(df_res_aux, li_bc > 1))
bc_der <- nrow(filter(df_res_aux, ls_bc < 1))

norm_cob <- nrow(filter(df_res_aux, li_normal <= 1, 1 <= ls_normal))
per_cob <- nrow(filter(df_res_aux, li_per <= 1, 1 <= ls_per))
bc_cob <- nrow(filter(df_res_aux, li_bc <= 1, 1 <= ls_bc))

cat("Normal:\t")
cat(norm_izq/500, ", ", norm_der/500, ", ", norm_cob/500)
cat("\nPer:\t")
cat(per_izq/500, ", ", per_der/500, ", ", per_cob/500)
cat("\nBC:\t")
cat(bc_izq/500, ", ", bc_der/500, ", ", bc_cob/500)
cat("\n")


p_norm <- ggplot() + 
	geom_line(data = df_res_aux, aes(x = prueba, y = li_normal, color = "blue")) +
	geom_line(data = df_res_aux, aes(x = prueba, y = ls_normal, color = "red")) + 
	xlab("") + ylab("Beta") + scale_color_discrete(name = "Intervalos", labels = c("li", "ls")) + 
	ggtitle("Variación de los Intervalos Para Cada Método") + theme(plot.title = element_text(hjust = 0.5))
p_per <- ggplot() + 
	geom_line(data = df_res_aux, aes(x = prueba, y = li_per, color = "blue")) +
	geom_line(data = df_res_aux, aes(x = prueba, y = ls_per, color = "red")) + 
	xlab("") + ylab("Beta") + scale_color_discrete(name = "Intervalos", labels = c("li", "ls"))

p_bc <- ggplot() + 
	geom_line(data = df_res_aux, aes(x = prueba, y = li_bc, color = "blue")) +
	geom_line(data = df_res_aux, aes(x = prueba, y = ls_bc, color = "red")) + 
	xlab("Intervalo") + ylab("Beta") + scale_color_discrete(name = "Intervalos", labels = c("li", "ls"))
multiplot(p_norm, p_per, p_bc, cols = 1)


# 1. Generar un número aleatorio U, tal que U∈(0,1).
# 2. Inicializar: i=0, p=e−λ, F=p.
# 3. Si U<F, definir X=i y parar.
# 4. p=λp/(i+1), F=F+p, i=i+1.
# 5. Volver a 3.
# Poisson usando Inversión
rBinomialI <- function(n, p)
{
  U <- runif(1)
  i <- 0
  p_i <- (1 - p) ^ n
  P <- p_i
  # cat("U: ", U, ",\tp_i: ", p_i, ",\tP: ", P , "\n")
  while(U >= P){
    i <- i + 1
    p_i <- (p_i / (1 - p) ) * (n - i + 1) * p / i 
    P <- P + p_i
 	# cat("U: ", U, ",\tp_i: ", p_i, ",\tP: ", P , "\n")
  }
  i
}


set.seed(221285)
binomial_aux <- rerun(10000, rBinomialI(10, 0.3)) %>% flatten_dbl()
df_binomial_aux <- as.data.frame(0:10)

df_binomial_aux <- add_column(df_binomial_aux, 0)
df_res <- as_data_frame(table(binomial_aux))
for (i in 1:length(df_res$n))
{
	df_binomial_aux[i,2] <- df_res$n[i]
}
# df_binomial_aux <- mutate(df_binomial_aux, table(binomial_aux))
rbinom_v <- rbinom(10000, 10, 0.3)
df_binomial_aux <- mutate(df_binomial_aux, rbin = table(rbinom_v))
names(df_binomial_aux) <- c("i", "MTID", "rbin")
df_tot_bin <- mutate(df_tot_bin, rbin = rbinom_v, i = 1:10000)
ggplot() + 
	geom_point(data = df_tot_bin, 
		aes(x = i, y = binomial_aux, color = "red")) + 
	geom_point(data = df_tot_bin, 
		aes(x = i, y = rbin, color = "blue")) +
	xlab("Iteración") + ylab("Valor") + 
	scale_color_discrete(name = "Método", labels = c("MTID", "rbinom"))

ggplot() + 
	geom_point(data = df_binomial_aux, 
		aes(x = i, y = MTID, color = "red")) + 
	geom_point(data = df_binomial_aux, 
		aes(x = i, y = rbin, color = "blue")) +
	xlab("Iteración") + ylab("Valor") 

	+ 
	scale_color_discrete(name = "Método", labels = c("MTID", "rbinom"))





