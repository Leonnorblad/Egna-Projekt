# Laddar in paket
require("tidyverse")
require("lubridate")

# Skapar en funktion som beräknar vad som krävs för att få tillbaka värdet vid en börskrash
value_func<-function(value_before, # Värdet innan krash
                     value_now, # Värdet efter krash
                     ang_stock_ret_percent){ # Snittuppgång på börsen
  # Om differensen mellan värdet innan och värdet nu är mindre än 0 har man inte gjort en förlust
  if (value_before-value_now<0){
    print(paste0("Du har inte gjort en förlust!"))
  } else {
    # Beräknar antalet procent som värdet har minskat med  
    down_procent<-((value_before-value_now)/value_before)*100
    # Beräknar uppgången som krävs för att återfå samma värde
    up_for_same<-((value_before/value_now)-1)*100
    # Beräknar förändringsfaktorn vid den angivna snittuppgången
    ff<-((ang_stock_ret_percent/100)+1)
    # Beräknar antal år som krävs för att få tillbaka värdet
    year_to_back<-log(value_before/value_now)/log(ff)
    # Tar fram datumet då värdet ska vara tillbaka
    date_back<-today()+days((round(year_to_back*365)))
    # Printar resultaten
    print(paste0("Värdet på ditt innehav har minskat med ", round(down_procent,2), " procent"))
    print(paste0("För att återfå samma värde behövs en uppgång på ", round(up_for_same,2), " procent"))
    print(paste0("Med en snittuppgång på ", ang_stock_ret_percent, " procent så kommer du kommer få tillbaka värdet om ca ", round(year_to_back,1), " år"))
    print(paste0("Vilket är ", weekdays(date_back),"en den ", day(date_back)," ", month(date_back, label=TRUE, abbr=FALSE)," ", year(date_back)))
  }
}

# Exempel på värden som kan matas in
value_func(value_before = 10000, # 10 000 kr innan krash
           value_now = 6000, # 6 000 kr efter krash
           ang_stock_ret_percent=7 # Med en snittuppgång på 7% per år
)

