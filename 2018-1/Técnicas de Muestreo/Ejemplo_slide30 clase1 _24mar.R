## Técnicas de Muestreo
## Ejemplo Slide 30

## 24/03/2018

rm (list = ls())

pp = 0 # creación de variable
for (x2 in 1:30)
	{
	for (x1 in 0:min(20,30-x2))
		{pp = pp + 8*(x1/(x1+x2)) * choose (20,x1)*choose(80,x2)*choose(900,30-x1-x2)/choose(1000,30)
		}
	}
pp