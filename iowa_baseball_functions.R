#packages
library(ggplot2)
#library(plotly)

#read in data 
baseball_data<-read.csv('iowa_baseball.csv')

                   #CREATE FUNCTIONS 


#### creates a summary of a specific pitcher. Only pitcherid or pitcher name is required 
pitcher_sum <- function(PitcherId,BatterSide,Date,Semester){
  
  #subset the data by name or id 
  all_pitches<-baseball_data[which(baseball_data$PitcherId ==PitcherId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Pitcher ==PitcherId),]
  }
  
  
  #if extra arguments are provices then subset the data further 
  if(!missing(BatterSide)) {all_pitches<- all_pitches[which(all_pitches$BatterSide ==BatterSide),]  }
  if(!missing(Date)) {all_pitches<- all_pitches[which(all_pitches$Date ==Date),]  }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  
  
  #create columns
 
  
  total<-  tapply(all_pitches$PitcherId,all_pitches$TaggedPitchType, length)
  speed<-  tapply(all_pitches$RelSpeed,all_pitches$TaggedPitchType, mean)
  spin<-   tapply(all_pitches$SpinRate ,all_pitches$TaggedPitchType, mean)
  

  return(na.omit(data.frame(total,speed,spin)))
  
}


#### creates a summary of a specific batter. Only batterid or batter name is required 
batter_sum <- function(BatterId,PitcherThrows,Date,Semester){
  
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  
  
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows)) {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }
  if(!missing(Date)) {all_pitches<- all_pitches[which(all_pitches$Date ==Date),]  }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  
  
  #create columns
  total<-  tapply(all_pitches$BatterId,all_pitches$TaggedPitchType, length)
  exit_velo<-  tapply(all_pitches$ExitSpeed,all_pitches$TaggedPitchType,function(x){median(x,na.rm = TRUE)})
  
  
  is_contact<-function(x){  
  if( x=='InPLay'  | x=='FoulBall'){1}
  else if(x =='StrikeSwinging'){0}
 
   else{NA}
  }

  is_groundball<-function(x){  
    if( x=='GroundBall'  ){1}
    else if(x !='Undefined'){0}
    else{NA}
  }  
  
  all_pitches$contact<- sapply(all_pitches$PitchCall,is_contact)
  all_pitches$groundball<- sapply(all_pitches$HitType,is_groundball)
  
  swing_strike_perc<-round(1-  tapply(all_pitches$contact ,all_pitches$TaggedPitchType,function(x){mean(x,na.rm = TRUE)}),2)*100
  groundball_perc<-round(tapply(all_pitches$groundball,all_pitches$TaggedPitchType,function(x){mean(x,na.rm = TRUE)}),2)*100
  
  return(data.frame(total,exit_velo,swing_strike_perc,groundball_perc))
  
}



##### creates a spray chart for a specifc hitter. only batterId or batter name is requried 
spray_chart <- function(BatterId,PitcherThrows){
  
  
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows)) {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }
  

  #removes foul hits and adds a new column that makes plotting easier using trig 
   all_pitches<-all_pitches[which(abs(all_pitches$Bearing)<=45),]
   all_pitches$y_Distance<-all_pitches$Distance * cos((all_pitches$Bearing *pi) /180) 
   all_pitches$X_Distance<-all_pitches$Distance * sin((all_pitches$Bearing *pi) /180) 
  
   #pythag to create bases 
   bases<-data.frame(x=c(0,90/sqrt(2),0,-90/sqrt(2),0),
                     y=c(0,90/sqrt(2),2*90/sqrt(2),90/sqrt(2),0 ))
   
  p<-ggplot(all_pitches,aes(X_Distance,y_Distance))+
    geom_point(aes(colour=PlayResult,shape=HitType))+
    coord_equal()+
    geom_path(aes(x=x,y=y),data=bases)+
    geom_segment(x=0,xend=300,y=0,yend=300)+
    geom_segment(x=0,xend=-300,y=0,yend=300)

  
  return(p)
  
  
}

###build a lauch angle histogram
LA_plot <- function(BatterId,PitcherThrows,Semester){
  #subset data by either batter id or batter name 
  all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows)) {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  
  p<-ggplot(data=all_pitches)+
    geom_histogram(binwidth = 5,aes(Angle,fill=I('blue'),color=I('red') ))+
    scale_x_continuous('Angle',seq(-50,70,5))
  return(p)
  
}

#builds a variety of different heatmaps 
batter_heatmap<-function(PlayerId,measure,PitcherThrows,BatterSide,Semester){
  #subset data by either player id or player name 
  all_pitches<-baseball_data[which(baseball_data$PitcherId == PlayerId),]
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Pitcher ==PlayerId),]
  }
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$Batter ==BatterId),]
  }
  if(nrow(all_pitches)==0){
    all_pitches<-baseball_data[which(baseball_data$BatterId ==BatterId),]
  }
  

  
  
  
  #if extra arguments are provided then subset the data further 
  if(!missing(PitcherThrows)) {all_pitches<- all_pitches[which(all_pitches$PitcherThrows ==PitcherThrows),]  }
  if(!missing(Semester)) {all_pitches<- all_pitches[which(all_pitches$Semester ==Semester),]  }
  if(missing(measure)){measure<-'ExitSpeed'}
  if(!missing(BatterSide)) {all_pitches<- all_pitches[which(all_pitches$BatterSide ==BatterSide),]  }
  
  
  zone_rates<-data.frame()
  zone_sequence_x<-seq(-2.2,2.2,.4)
  zone_sequence_y<-seq(.2,5,.5)
  total_zone<-length(zone_sequence_x)*length(zone_sequence_y)
  zone_rates[1:total_zone,"zones"]<-as.vector(1:total_zone) 
  zone_rates$x<-0
  zone_rates$y<-0
  #creates the zones
  cnt<-1
  for(i in zone_sequence_y){
    for(j in zone_sequence_x){
      
      zone_rates$x[cnt]<-j
      zone_rates$y[cnt]<-i
      
      cnt<-cnt+1;        
      
    }
    
  }
  find_zone<-function(x,y){

 
    cnt<-1

    call<-NA
    for(i in zone_sequence_y){
      for(j in zone_sequence_x){
        if(y<i & y >=i-.2 & x<j & x>=j-.2 ){
          call<-cnt
          return(call)
        }
        cnt<-cnt+1
      }
    }
    return(call)
  }

all_pitches<-all_pitches[which(!is.na(all_pitches$PlateLocHeight) & !is.na(all_pitches$PlateLocSide)   ),]
all_pitches$zone<-mapply(find_zone,all_pitches$PlateLocSide,all_pitches$PlateLocHeight)
    

if(missing(measure) | measure=='ExitSpeed' ){
zone_rates$ExitSpeed<-sapply(zone_rates$zones,function(x) mean(all_pitches$ExitSpeed[which(all_pitches$zone==x)],na.rm=TRUE) )
fill<-geom_tile(aes(fill=ExitSpeed))
}
else if (measure=='Frequency'){
  zone_rates$Frequency<-sapply(zone_rates$zones,function(x) length(all_pitches$ExitSpeed[which(all_pitches$zone==x)]) )
  fill<-geom_tile(aes(fill=Frequency))
}
else if (measure=='Iso'){

  find_iso<-function(x){
  all_pitches<-all_pitches[which(all_pitches$zone==x),]  
   ab<-length(which(all_pitches$PlayResult %in% c('Single','Double','Triple','HomeRun','Out')  ))
   if(is.na(ab)){ return(0)} 
   else {
     return(
      length(which(all_pitches$PlayResult=='Double')) 
    + (2*length(which(all_pitches$PlayResult=='Triple'))) 
    + (3*length(which(all_pitches$PlayResult=='HomeRun'))) 
    / ab
     )}
  }
  
  zone_rates$Iso<-sapply(zone_rates$zones,find_iso )
  fill<-geom_tile(aes(fill=Iso))
}


##estimates of borders of the strikezone
topKzone<- 3.5
botKzone<-1.6
inKzone <- -.95
outKzone <- 0.95






p<-ggplot(zone_rates,aes(x,y))+
  fill+
  geom_segment(x=inKzone,xend=inKzone,y=botKzone,yend=topKzone)+
  geom_segment(x=outKzone,xend=outKzone,y=botKzone,yend=topKzone)+
  geom_segment(x=inKzone,xend=outKzone,y=botKzone,yend=botKzone)+
  geom_segment(x=inKzone,xend=outKzone,y=topKzone,yend=topKzone)+
 scale_fill_gradient(low = "green", high = "red")

#p<-plot_ly(x=zone_rates$x,y=zone_rates$y,z=zone_rates$ExitSpeed,type='contour',contours=list(coloring='heatmap'))  

  return(p)
}  
  


 



