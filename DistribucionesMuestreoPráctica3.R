
################################################################################
######################### DISTRIBUCIONES EN EL MUESTREO ########################




## DISTRIBUCION t-student (n grados de libertad)

dt(x, df=n) ## f(x)
pt(q, df=n) ## F(x)=P(X<=x)
qt(p, df=n) ## calcular el cuantil sabiendo la probabilidad
rt(n, df=n) ## generar una muestra de tamaño n a partir de una distribucion t

## DISTRIBUCION CHI-CUADRADO (df=n grados de libertad)

dchisq(x, df=n) ## f(x)
pchisq(q, df=n) ## F(x)=P(X<=x)
qchisq(p, df=n) ## calcular el cuantil sabiendo la probabilidad
rchisq(n, df=n) ## generar una muestra de tamaño n a partir de una distribucion Chi-cuadrado


## DISTRIBUCION F (df1=n1 y df2=n2  grados de libertad)


df(x, df1=n1, df2=n2) ## f(x)
pf(q, df1=n1, df2=n2) ## F(x)=P(X<=x)
qf(p, df1=n1, df2=n2) ## calcular el cuantil sabiendo la probabilidad
rf(n, df1=n1, df2=n2) ## generar una muestra de tamaño n a partir de una distribucion F

