lapply(c(prodID$tcigte, prodID$ecigte),
       function(id){
         
         lapply(2018:2020, function(year){
           
           subsetToPre18Sample(knd, year, id)
           
         })
         
       })
