library(jsonlite)
library(httr)
library(data.table)
library(fpp2)
library(MLmetrics)
install.packages("MLmetrics")
get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32939029":2.4,"4066298":2.4,"6676673":2.4,"7061886":2.4,"85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://167.172.183.67'

u_name = "Group21"
p_word = "N3OB8glqYqbs0aFT"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

str(data)
View(data)

data31<- data[product_content_id=="31515569",]
View(data31)
ts31 <- ts(data31[251:nrow(data31),4], start = c(2020,5), frequency = 365)


data32<- data[product_content_id=="32939029",]
View(data32)
ts32 <- ts(data32[278:nrow(data32),4], start = c(2020,32), frequency = 365)

data39<- data[product_content_id=="3904356",]
View(data39)
#This product is not included in the analysis because it is an off-season product

data40<- data[product_content_id=="4066298",]
View(data40)
ts40 <- ts(data40[134:nrow(data40),4], start = c(2019,253), frequency = 365)

data59<- data[product_content_id=="5926527",]
View(data59)
ts59 <- ts(data31[,4], start = c(2019,120), frequency = 365)

data66<- data[product_content_id=="6676673",]
View(data66)
ts66 <- ts(data66[134:nrow(data66),4], start = c(2019,253), frequency = 365)

data70<- data[product_content_id=="7061886",]
View(data70)
ts70 <- ts(data70[89:nrow(data70),4], start = c(2019,208), frequency = 365)

data85<- data[product_content_id=="85004",]
View(data85)
ts85 <- ts(data85[,4], start = c(2019,120), frequency = 365)

fets <- function(x, h) {
  forecast(ets(x), h = h)
}
farima <- function(x, h) {
  forecast(auto.arima(x), h=h)
}

# Compute CV errors for ETS as e1
e1 <- tsCV(ts31, fets, h=2)
# Compute CV errors for ARIMA as e2
e2 <- tsCV(ts31, farima, h=2)
# Find MSE of each model class
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

e1 <- tsCV(ts32, fets, h=2)
e2 <- tsCV(ts32, farima, h=2)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

e1 <- tsCV(ts40, fets, h=2)
e2 <- tsCV(ts40, farima, h=2)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

e1 <- tsCV(ts59, fets, h=2)
e2 <- tsCV(ts59, farima, h=2)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

e1 <- tsCV(ts66, fets, h=2)
e2 <- tsCV(ts66, farima, h=2)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

e1 <- tsCV(ts70, fets, h=2)
e2 <- tsCV(ts70, farima, h=2)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)

e1 <- tsCV(ts85, fets, h=2)
e2 <- tsCV(ts85, farima, h=2)
mean(e1^2, na.rm=TRUE)
mean(e2^2, na.rm=TRUE)


fit31 <- ets(ts31)
f31 <- forecast(fit31, h=2)
f31
plot(f31)

fit32 <- auto.arima(ts32)
f32 <- forecast(fit32, h=2)
f32
plot(f32)

fit40 <- auto.arima(ts40)
f40 <- forecast(fit40, h=2)
f40
plot(f40)

fit59 <- ets(ts59)
f59 <- forecast(fit59, h=2)
f59
plot(f59)

fit66 <- auto.arima(ts66)
f66 <- forecast(fit66, h=2)
f66
plot(f66)

fit70 <- auto.arima(ts70)
f70 <- forecast(fit70, h=2)
f70
plot(f70)

fit85 <- auto.arima(ts85)
f85 <- forecast(fit85, h=2)
f85
plot(f85)

f31
f32
f39=0
f40
f59
f66
f70
f85

predictions=unique(data[,list(product_content_id)])
predictions
predictions[1,forecast:=0]
predictions[2,forecast:=0]
predictions[3,forecast:=0]
predictions[4,forecast:=0]
predictions[5,forecast:=0]
predictions[6,forecast:=0]
predictions[7,forecast:=0]
predictions[8,forecast:=0]

send_submission(predictions, token, url=subm_url, submit_now=F)


