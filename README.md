[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=EL-BID_mobilidadeTurismo&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=EL-BID_mobilidadeTurismo)

# PoC Ciclo de Mobilidade

![Figura 1: Tela inicial Ciclo de Mobilidade](www/screen0.png)

## Tabela de Conteúdo:
---

- [Descrição](#descricao)
- [Guia do usuário](#guia-do-usuario)
- [Guia de instalação](#guia-de-instalacao)
- [Autores](#autores)
- [Licença](#licenca)

## Descrição <a name="descricao"></a>
---

O trânsito da cidade do Recife foi identificado como o pior do Brasil e um dos 15 piores do mundo. Recife é a capital brasileira que mais teve congestionamentos no ano de 2021.  Alguns projetos vêm sendo desenvolvidos pela prefeitura em busca de soluções para mitigar esse problema, como por exemplo a expansão da malha cicloviária da cidade. Atualmente a cidade do Recife possui o Plano Diretor Cicloviário, um plano que define as diretrizes de implementação da expansão da malha cicloviária na cidade e região metropolitana. 

A PoC Ciclo de Mobilidade utiliza o conjunto de dados **Strava Metro**, informações públicas como **OpenStreetMap** e conjunto de dados sobre a **malha cicloviária** da cidade do Recife para construir uma solução para complementar as informações do Plano Diretor Cicloviário e auxiliar no debate acerca da mobilidade na cidade do Recife. Utilizando esses conjuntos de dados é possível construir diversos indicadores, tais como:

- Nível de cobertura por malha cicloviária das vias com tráfego;
- Nível de cobertura por malha cicloviária por nível de tráfego;
- Lista de  ruas/avenidas com tráfego alto e não cobertas por malha cicloviária.


## Guia do Usuário <a name="guia-do-usuario"> </a>
---

A tela inicial da aplicação mostra a solução de **ciclo de mobilidade**. O primeiro conjunto de informações apresentados são os índices de cobertura da malha cicloviária sobre os trechos cicláveis.
Os índices são divididos por nível de tráfego nos trechos e categorizados por:

1. **% total sem cobertura**;
2. Quanto do **% total sem cobertura** possui previsão do Plano Diretor Cicloviário (C/ PDC);
3. Quanto do **% total sem cobertura** ainda sem previsão do Plano Diretor Cicloviário (S/ PDC).

Como demonstrado na Figura 4, é possível escolher a variável utilizada para fazer toda a análise. 

![Figura 4: Ciclo de Mobilidade](www/screen1.png)

O segundo conjunto de informações demonstrados são os rankings da ruas sem cobertura agrupados por nível de tráfego. As ruas são ordenadas em forma decrescente a partir do volume de tráfego. Assim como anteriormente, é possível observar o ranking das ruas que possuem previsão do PDC e sem previsão do PDC.

![Figura 5: Ciclo de Mobilidade](www/screen2.png)

A seguir, é demonstrado o mapa da cidade do Recife com todo o tráfego reportado (categorizado por nível de tráfego), assim como a malha cicloviária e o Plano Diretor Cicloviário. É possível selecionar quais *layer* observar no mapa. 

![Figura 6: Ciclo de Mobilidade](www/screen3.png)

Por fim, há uma seção de **Upload** de arquivos no qual a prefeitura do Recife pode realizar as atualizações de toda a aplicação com o carregamento de novas versões dos arquivos.

![Figura 8: Upload](www/screen4.png)

## Guia de Implementação  <a name="guia-do-implementacao"> </a>
---
A implementação da aplicaçãoé bastante simples e seguirá três passos simples. São eles:

1. Download do software R e as bibliotecas necessárias;
2. Download dos códigos-fonte da aplicação;
3. Execução da aplicação.

## 1. Download de software:

A aplicação foi escrita na linguagem [R](https://www.r-project.org/) e utilizando as seguintes bibliotecas para a criação do dashboard:

- shiny;
- shinydashboard;
- shinydashboardPlus;
- shinyWidgets;
- data.table;
- tidyverse;
- lubridate;
- leaflet;	
- arrow;
- fresh;
- DT;
- sf;
- rgdal;
- raster;
- geojsonsf;
- waiter;

Após a instalação do R e das bibliotecas listadas anteriormente, o próximo é obter os arquivos da aplicação.

## 2. Clonando repositório remoto: 

É possível realizar o download dos arquivos da aplicação através do repositório remoto do GitHub. Através da linha de commando:

```sh
git clone <url>
```

## 3. Executando a aplicação:

Com os bancos de dados no formato apropriado e salvo nas respectivas pastas, basta executar o arquivo `app.R`. Através da linha de comando:

```sh
Rscript app.R
```


## Autores <a name="autores"></a>
---

- [Cleyton Farias](mailto:cleytonfarias@outlook.com "e-mail");

- [Rubens Lopes](mailto:lps.rubens@gmail.com "e-mail");


## Licença
---
