
#######################################################################################################
# função Pcov2 - calcula as autocovariâncias dos grupos de rotação
#######################################################################################################
Pcov2  =  function(v,lag){
  
  T  = length(v)
  Pcov  =  rep(0,lag)
  for (i in 0:(lag-1)) { # para i de 0 ao lag-1, autocovariâncias 0 até 23
    Pcov[i+1] = sum((v[1:(T-i)] - mean(v[1:(T-i)])) * (v[(1+i):T] - mean(v[(1+i):T])))/(T-i) 
    # alterado de T para T-i conforme desenvolvimento da eq. 5.2 livro Denise - p.70
    }
  return(Pcov)
}

#######################################################################################################
#######################################################################################################
# função Pcov - calcula as autocovariâncias dos grupos de rotação
#######################################################################################################
Pcov  =  function(v,lag){
  
  T  = length(v)
  Pcov  =  rep(0,lag)
  for (i in 0:(lag-1)) { # para i de 0 ao lag-1, autocovariâncias 0 até 4
    Pcov[i+1] = sum((v[1:(T-i)] - mean(v[1:(T-i)])) * (v[(1+i):T] - mean(v[(1+i):T])))/(T) 
  }
  return(Pcov)
}

#######################################################################################################

#função para resolver o algoritmo de Durbin-Levinson
DL <- function(c,p){
  #Ver Wei pg.23 (pg. 43 do pdf)
  par <- matrix(0,p,p)
  #iniciando
   par[1,1] <- c[2]/c[1]
  if (p>1){
  for (i in 2:p) {
      par[i,i] <- (c[i+1]-sum(par[1:(i-1),i-1]*rev(c[2:i])))/(1-sum(par[1:(i-1),i-1]*c[2:i]))
      for (j in 1:(i-1))par[j,i]=par[j,i-1]-par[i,i]*par[i-j,i-1]
  }
  }
  return(par[,p])
}

#######################################################################################################
#fun??o para criar os valores da FACP 
facp <- function(c,p){
  #Usa-se o algoritmo de Durbin-Levinson para montar as 
  #facp atrav?s da autocorrela??o dos erros amostrais
  #estimadas atrav?s dos pseudos-erros
  facp <- rep(0,p)
  facp[1] <- c[2]
  for (i in 2:p) facp[i] <- DL(c,i)[i]
  return(facp)
}

#######################################################################################################
teste <- function(facp,n=72){1-pchisq(n*facp^2,1)}

#######################################################################################################
P0e <- function(pseudoErro){
	p <- pseudoErro$ordem
	P0e <- diag(0,p)
	var <- pseudoErro$var
	autoCOV <- pseudoErro$calculos[1:p,p+3]
	coef <- pseudoErro$coef
	
	defasagem <- diag(0,p)
	rownames(defasagem) <- c(1:p)
	colnames(defasagem) <- c(1:p)
	defasagem[1,1] <- 0
	if (p>1) {for (i in 2:p) defasagem[i,i:p] <- c(1:(p-i+1))

	
	M_coef <- diag(0,p)
	rownames(M_coef) <- c(1:p)
	colnames(M_coef) <- c(1:p)
	M_coef[1,1] <- 1
	
	for (i in 2:p) M_coef[i,i:p] <- coef[i:p]

	for (i in 1:p){
		for (j in 1:p){
			resultado <- 0
			for (k in 1:p) {
			for (w in 1:p) {resultado <- resultado+M_coef[i,k]*M_coef[j,w]*autoCOV[abs(defasagem[i,k]-defasagem[j,w])+1]}}
			P0e[i,j] <- resultado	
		}
	}
	}else{P0e[1,1] <- autoCOV}
	return(P0=P0e)
}

###################################################################################################
facp_acf <- function(fac,lag){
  facp = matrix(0, lag, 1)
  matrix1 = diag(rep(1,2)) 
  
  for (i in 1:lag){
    if (i == 1){
      facp[i]= fac[i+1]}
    else if (i == 2){
      matrix1[i,i-1] = fac[i]
      matrix1[i-1,i] = fac[i]
      matrixden = matrix1
      matrixnum= matrixden
      matrixnum[,i]=fac[1:i+1]
      facp[i]= det(matrixnum)/det(matrixden)}
    else{
      matrix1 = cbind(matrix1, rev(fac[2:(i)]))
      matrix1 = rbind(matrix1, rev(fac[1:(i)]))
      matrixden = matrix1
      matrixnum= matrixden
      matrixnum[,i]=fac[1:i+1]
      facp[i]= det(matrixnum)/det(matrixden)
    }
  }
  return(facp)
  print(facp)
}


###################################################################################################
#Homocedasticidade - Teste H
H <-  function(vt){
  n <- length(vt)
  h <- round((n)/3)
  H <- sum(vt[(n-h+1):n]^2)/sum(vt[1:h]^2)
  return(cat("H = ",H,", p-value = ",1-pf(H,h,h)))
  
}
