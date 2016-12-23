library(SPARQL)
library(plotly)
library(leaflet)

endpoint <- "http://localhost:3030/5213100006/sparql"
  
query <-
"PREFIX pem: <http://www.pemerintah.go.id/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbp: <http://dbpedia.org/property/>
PREFIX apbd: <http://www.apbd.go.id/>

SELECT ?pendapatan ?populasi
{
  ?temp a pem:provinsi .
  ?kondisi a pem:kondJenisAPBD .
  ?temp pem:kondAlokasiDana ?kondisi .
  ?kondisi pem:nilaiAnggaran ?pendapatan .
  ?temp owl:sameAs ?out .
  SERVICE <http://dbpedia.org/sparql>
  {SELECT * WHERE{
  ?out rdfs:label ?provinsi .
  ?out dbo:populationTotal ?populasi .
  FILTER(LANG(?provinsi) = '' || LANGMATCHES(LANG(?provinsi), 'en'))
  }  
}
}"

qd <- SPARQL(endpoint, query)
df <- qd$results

summary(df)
print(df)

result <- kmeans(df, 6)
result

result2 <- data.frame(df, result$cluster)
result2

#ggplot(result2, aes(pendapatan, populasi, color = result$cluster) ) + geom_point(stat="identity")

#clusplot(df2, result1$cluster, color=TRUE, shade=TRUE, 
#         labels=3, lines=0)

query2 <-
"PREFIX pem: <http://www.pemerintah.go.id/>
PREFIX owl: <http://www.w3.org/2002/07/owl#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
PREFIX dbo: <http://dbpedia.org/ontology/>
PREFIX dbp: <http://dbpedia.org/property/>
PREFIX apbd: <http://www.apbd.go.id/>
PREFIX foaf: <http://xmlns.com/foaf/0.1/>
PREFIX prov: <http://www.w3.org/ns/prov#>

SELECT ?nama_provinsi ?lat ?long ?area ?link
{
  ?temp a pem:provinsi .
  ?temp pem:lat ?lat .
  ?temp pem:long ?long .
  ?temp rdfs:label ?nama_provinsi .
  ?kondisi a pem:kondJenisAPBD .
  ?temp pem:kondAlokasiDana ?kondisi .
  ?kondisi pem:nilaiAnggaran ?pendapatan .
  ?temp owl:sameAs ?out .
  SERVICE <http://dbpedia.org/sparql>
  {SELECT * WHERE{
  ?out rdfs:label ?provinsi .
  ?out dbo:populationTotal ?populasi .
  ?out dbp:areaTotalKm ?area .
  ?out prov:wasDerivedFrom ?link
  FILTER(LANG(?provinsi) = '' || LANGMATCHES(LANG(?provinsi), 'en'))
  }
  }
}"
  
qd2 <- SPARQL(endpoint, query2)
df2 <- qd2$results

result3 <- data.frame(df2, result2)
result3

d <- result3[sample(nrow(result3), 34), ]
plot_ly(d, x = ~populasi, y = ~as.numeric(pendapatan), color = ~result.cluster,
        size = ~result.cluster, text = ~paste("Nama: ", nama_provinsi))

result3$lat <- as.numeric(result3$lat)
result3$long <- as.numeric(result3$long)
inisiasi <- result3[!is.na(result3$lat) & !is.na(result3$long),]

map <- leaflet(data = inisiasi) %>%
  addTiles()
map

link2map <- paste(result3$link)
popupLink <- paste0("<a href=\"",
                    link2map,
                    "\" target=\"_blank\">",
                    result3$nama_provinsi,
                    "</a>")

map <- clearMarkers(map) %>%
  addMarkers(lng = ~long, lat = ~lat, popup = paste(popupLink,
            "<br>", "Latitude: ", result3$lat,
            "<br>", "Longitude: ", result3$long,
            "<br>", "Luas Area: ", result3$area,
            "<br>", "Pajak Daerah: ", result3$pendapatan,
            "<br>", "Populasi: ", result3$populasi,
            "<br>", "Cluster: ", result3$result.cluster
            ), clusterOptions = markerClusterOptions())
map


