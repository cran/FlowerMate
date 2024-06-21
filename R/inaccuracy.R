inaccuracy <-
function(input=NULL,useonly.dim=c("x","y","z"), useonly.vert="all", verbose=FALSE,na.rm=FALSE,intramorph=FALSE)
{

sanchez=TRUE
inputComplete<-input

if(na.rm)
warning("Warning: there are missing data for some sex organs and/or dimensions. The values retrieved may not be comparable accross populations.")

#### Checking potential errors:  column names (they must match the "mandatory" names)
if(any(is.na(match(c("pop_code","morph","ID_indiv","ID_sexorg","sexorg"),colnames(inputComplete)))))
stop('\nError: Please check column names to match the following: "pop_code","morph","ID_indiv","ID_sexorg","sexorg"\n')

## Removing variables X,Y,Z as defined by user
byuser<- c("x","y","z")[-match(useonly.dim,c("x","y","z"))]

if(length(byuser)!=0)
	inputComplete[,byuser]<-0

## Starting computation
OUTPUT_FINAL <- c()

POPS<-unique(inputComplete$pop_code)

for (i.pops in 1:length(POPS))
	{
	input <- inputComplete[which(inputComplete$pop_code==POPS[i.pops]),]

	hayX <- !is.na(match("x",colnames(input)))
	hayY <- !is.na(match("y",colnames(input)))
	hayZ <- !is.na(match("z",colnames(input)))


	if(!any(c(hayX ,
	hayY,
	hayZ)))
	break("Error: \nPlease check trait colum names to fit input requirements (x, y, and z)\n")

	## Now check if X, Y and Z have any data (not only 0)

	if(hayX)
		if(!any(input$x!=0))
			hayX<-FALSE
	if(hayY)
		if(!any(input$y!=0))
			hayY<-FALSE
	if(hayZ)
		if(!any(input$z!=0))
			hayZ<-FALSE

	## Checking morph (only L, S)
	if(any(is.na(match(unique(input$morph),c("L","S","M")))))
	stop('\nerror: Check morph values to fit input requirements (either "L" and "S" or "L", "M", and "S"\n')

	## Checking sexorg (only ST, AN)

	SEXORG<-input$sexorg
	for (i in 0:9)
		SEXORG<-gsub(i,"",SEXORG)

	if(any(is.na(match(SEXORG,c("AN","ST")))))
		stop("Check sexorg column values (must be AN/ST, or ANx/STx where x represents a numeric).")

	## If only particular ST and AN must be used...
	## ... select the rows...
	if(
	length(useonly.vert)!=1 |
	tolower(useonly.vert)[1] !="all")
		{
		select<-c()
		for (i.use in useonly.vert)
		select<-c(select,which(i.use== input$sexorg))
		input<-input[select,]
		}
	## Just in case you get an empty matrix
	if(nrow(input)==0)
		stop("Check useonly.vert values provided")

	## ...and change the names to make them all being ST and AN
	input$sexorg<-gsub("1","",input$sexorg)
	input$sexorg<-gsub("2","",input$sexorg)
	input$sexorg<-gsub("3","",input$sexorg)

	## Is it Dimorphic or trimorphic?
	Nmorfos <- length(unique(input$morph))

	## Every individual must show the same number of sexual organs
	if(length(unique(table(input$ID_indiv)))!=1)
	stop("\nThere are different number of sexual organs measured in different individuals. Please, provided an input with a constant number of sexual organs measured across individuals.\n")


## Dimorphic:
		if(Nmorfos==2)
		if(intramorph==FALSE)
		{
		nL=nrow(input[which(input$morph=="L"),])/(unique(table(input$ID_indiv)))
		nS=nrow(input[which(input$morph=="S"),])/(unique(table(input$ID_indiv)))


		ST.u<-subset(input[which(input$morph=="L" & input$sexorg=="ST"),])
		ST.d<-subset(input[which(input$morph=="S" & input$sexorg=="ST"),])
		AN.u<-subset(input[which(input$morph=="S" & input$sexorg=="AN"),])
		AN.d<-subset(input[which(input$morph=="L" & input$sexorg=="AN"),])


		###########################
		######### means
		###########################


		Mean.ST.u.x=mean(ST.u$x, na.rm=T)
		Mean.AN.u.x=mean(AN.u$x, na.rm=T)
		Mean.ST.u.y=mean(ST.u$y, na.rm=T)
		Mean.AN.u.y=mean(AN.u$y, na.rm=T)
		Mean.ST.u.z=mean(ST.u$z, na.rm=T)
		Mean.AN.u.z=mean(AN.u$z, na.rm=T)

		Mean.ST.d.x=mean(ST.d$x, na.rm=T)
		Mean.AN.d.x=mean(AN.d$x, na.rm=T)
		Mean.ST.d.y=mean(ST.d$y, na.rm=T)
		Mean.AN.d.y=mean(AN.d$y, na.rm=T)
		Mean.ST.d.z=mean(ST.d$z, na.rm=T)
		Mean.AN.d.z=mean(AN.d$z, na.rm=T)



		###########################
		#########variances
		###########################

		##############var3D.AN.d


		AN.d$centroidesv <- (AN.d$x-Mean.AN.d.x)^2 + (AN.d$y-Mean.AN.d.y)^2 + (AN.d$z-Mean.AN.d.z)^2 #eq. 22 part

		var3D.AN.d <- sum(AN.d$centroidesv)/ (nrow(AN.d)-1) #eq. 22


		##############var3D.AN.u

		AN.u$centroidesv <- (AN.u$x-Mean.AN.u.x)^2 + (AN.u$y-Mean.AN.u.y)^2 + (AN.u$z-Mean.AN.u.z)^2 #eq. 22 part

		var3D.AN.u <- sum(AN.u$centroidesv)/ (nrow(AN.u)-1)


		##############var3D.ST.d


		ST.d$centroidesv <- (ST.d$x-Mean.ST.d.x)^2 + (ST.d$y-Mean.ST.d.y)^2 + (ST.d$z-Mean.ST.d.z)^2 #eq. 22 part

		var3D.ST.d <- sum(ST.d$centroidesv)/ (nrow(ST.d)-1) #eq. 22


		##############var3D.ST.u

		ST.u$centroidesv <- (ST.u$x-Mean.ST.u.x)^2 + (ST.u$y-Mean.ST.u.y)^2 + (ST.u$z-Mean.ST.u.z)^2 #eq. 22 part

		var3D.ST.u <- sum(ST.u$centroidesv)/ (nrow(ST.u)-1)


		###########################
		#########inaccuracy high
		###########################

		Inac.u.xyz= (sqrt((Mean.AN.u.x-Mean.ST.u.x)^2+(Mean.AN.u.y-Mean.ST.u.y)^2+(Mean.AN.u.z-Mean.ST.u.z)^2))^2 + var3D.AN.u + var3D.ST.u

		###########################
		#########inaccuracy low
		###########################

		Inac.d.xyz= (sqrt((Mean.AN.d.x-Mean.ST.d.x)^2+(Mean.AN.d.y-Mean.ST.d.y)^2+(Mean.AN.d.z-Mean.ST.d.z)^2))^2 + var3D.AN.d + var3D.ST.d


		###########################
		##############PADO population average distance of all pooled sex organs to the origin
		###########################

		input$origindist <- sqrt(input$x^2 + input$y^2 + input$z^2)

		PADO <- sum(input$origindist)/ nrow(input)



		###########################
		#########total inaccuracy
		###########################

		TotalInac=Inac.u.xyz + Inac.d.xyz

		Mean2Stand.TotalInac=TotalInac/(PADO^2)


		Inac.perc.u=100*Inac.u.xyz/(Inac.u.xyz+Inac.d.xyz)
		Inac.perc.d=100*Inac.d.xyz/(Inac.u.xyz+Inac.d.xyz)

		OUTPUT<-matrix(c(Inac.u.xyz, Inac.d.xyz, Inac.perc.u, Inac.perc.d, TotalInac, Mean2Stand.TotalInac),
		dimnames=list(c("Inac.u", "Inac.d", "Inac.perc.u", "Inac.perc.d", "TotalInac", "Mean2Stand.TotalInac"),POPS[i.pops]))



		if(sanchez)
			{
			input<-input[,-c(match("origindist",colnames(input)))]

			## Sanchez index
			subsetST<-input[input$sexorg=="ST",]
			distST.2 = sqrt((subsetST$x)^2+(subsetST$y)^2+(subsetST$z)^2) 

			subsetAN<-input[input$sexorg=="AN",]
			distAN.2 = sqrt((subsetAN$x)^2+(subsetAN$y)^2+(subsetAN$z)^2)

			n.2= nrow(input[which(input$morph=="L" & input$sexorg=="ST"),]) # 	num rows morfo==L & sexorg==ST
			m.2= nrow(input[which(input$morph=="S" & input$sexorg=="AN"),]) # num rows morfo==S & sexorg==AN
			p.2= nrow(input[which(input$morph=="L" & input$sexorg=="AN"),]) #num rows morfo==L & sexorg==AN
			q.2= nrow(input[which(input$morph=="S" & input$sexorg=="ST"),]) #m.2= num rows morfo==S & sexorg==ST
			avdistAN.2= mean(distAN.2, na.rm=na.rm)
			avdistST.2= mean(distST.2, na.rm=na.rm)
			Xr.2=mean(c(avdistAN.2,avdistST.2), na.rm=na.rm)

### For UP level

			par1 <- input[which(input$"morph"=="L" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"morph"=="S" & input$"sexorg"=="AN"),]

			df.2u<-c()
			for (i.2 in 1:nrow(par1))
			for (j.2 in 1:nrow(par2))
				{
				df.2u<-rbind(df.2u,cbind(par1[i.2,],par2[j.2,]))
				}

			colnames(df.2u)<-c("pop_code_mum","morph_mum","IDmum_L","ID_stigma","sexorg_mum", "upstigmaX","upstigmaY","upstigmaZ","pop_code_dad","morph_dad", "IDdad_S", "ID_anther",
			"sexorg_dad","upantherX","upantherY","upantherZ")
			df.2u$dist.2u <- sqrt((df.2u$upstigmaX-df.2u$upantherX)^2+(df.2u$upstigmaY-df.2u$upantherY)^2+(df.2u$upstigmaZ-df.2u$upantherZ)^2)
			df.2u$distX.2u <-  df.2u$dist.2u/Xr.2
			rL.2u <- sum(df.2u$distX.2u, na.rm=na.rm)/(n.2*m.2)
			distXrL.2u <- (df.2u$distX.2u-rL.2u)^2
			sdrL.2u <- sqrt(sum(distXrL.2u, na.rm=na.rm)/(n.2*m.2))

## For DOWN level

			par1 <- input[which(input$"morph"=="S" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"morph"=="L" & input$"sexorg"=="AN"),]

			df.2d<-c()
			for (i.3 in 1:nrow(par1))
			for (j.3 in 1:nrow(par2))
			{
			df.2d<-rbind(df.2d,cbind(par1[i.3,],par2[j.3,]))
			}

			colnames(df.2d)<-c("pop_code_mum","morph_mum","IDmum_S","ID_stigma","sexorg_mum", "downstigmaX","downstigmaY","downstigmaZ","pop_code_dad","morph_dad", "IDdad_L", "ID_anther",
			"sexorg_dad","downantherX","downantherY","downantherZ")

			df.2d$dist.2d <- sqrt((df.2d$downstigmaX-df.2d$downantherX)^2+(df.2d$downstigmaY-df.2d$downantherY)^2+(df.2d$downstigmaZ-df.2d$downantherZ)^2)
			df.2d$distX.2d <- df.2d$dist.2d/Xr.2
			rS.2d <- sum(df.2d$distX.2d, na.rm=na.rm)/(p.2*q.2)
			distXrS.2d <- (df.2d$distX.2d-rS.2d)^2
			sdrS.2d <- sqrt(sum(distXrS.2d, na.rm=na.rm)/(p.2*q.2))  ###corrected sqrt

### Final estimates

			r.2 <- sqrt(rL.2u^2+rS.2d^2)
			sdr.2 <- (sdrL.2u+sdrS.2d)/2
			R.2 <- 1-(10*r.2*sdr.2)
			out.SA <- matrix(c(r.2,R.2),dimnames=list(c("r","R"),POPS[i.pops]))

			verbSanchez<- c(rL.2u, sdrL.2u, rS.2d, sdrS.2d, r.2, sdr.2, R.2)
			names(verbSanchez)<-c("rL", "sdrL", "rS", "sdrS", "r", "sdr", "R")

			}

		###### OUTPUTS
		ifelse(sanchez,
		OUTPUT <- rbind(matrix(c(nL,nS),dimnames=list(c("nL","nS"),POPS[i.pops])),OUTPUT,out.SA),
		OUTPUT <- rbind(matrix(c(nL,nS),dimnames=list(c("nL","nS"),POPS[i.pops])),OUTPUT)
		)
		}else{ ## This is for intramorph==TRUE
		
		nL=nrow(input[which(input$morph=="L"),])/(unique(table(input$ID_indiv)))
		nS=nrow(input[which(input$morph=="S"),])/(unique(table(input$ID_indiv)))

		ST.u<-subset(input[which(input$morph=="L" & input$sexorg=="ST"),])
		ST.d<-subset(input[which(input$morph=="S" & input$sexorg=="ST"),])
		AN.u<-subset(input[which(input$morph=="S" & input$sexorg=="AN"),])
		AN.d<-subset(input[which(input$morph=="L" & input$sexorg=="AN"),])

		
		###########################
		######### means
		###########################


		Mean.ST.u.x=mean(ST.u$x, na.rm=T)
		Mean.AN.u.x=mean(AN.u$x, na.rm=T)
		Mean.ST.u.y=mean(ST.u$y, na.rm=T)
		Mean.AN.u.y=mean(AN.u$y, na.rm=T)
		Mean.ST.u.z=mean(ST.u$z, na.rm=T)
		Mean.AN.u.z=mean(AN.u$z, na.rm=T)

		Mean.ST.d.x=mean(ST.d$x, na.rm=T)
		Mean.AN.d.x=mean(AN.d$x, na.rm=T)
		Mean.ST.d.y=mean(ST.d$y, na.rm=T)
		Mean.AN.d.y=mean(AN.d$y, na.rm=T)
		Mean.ST.d.z=mean(ST.d$z, na.rm=T)
		Mean.AN.d.z=mean(AN.d$z, na.rm=T)

		
		
		
		###########################
		#########variances
		###########################

		##############var3D.AN.d


		AN.d$centroidesv <- (AN.d$x-Mean.AN.d.x)^2 + (AN.d$y-Mean.AN.d.y)^2 + (AN.d$z-Mean.AN.d.z)^2 #eq. 22 part

		var3D.AN.d <- sum(AN.d$centroidesv)/ (nrow(AN.d)-1) #eq. 22


		##############var3D.AN.u

		AN.u$centroidesv <- (AN.u$x-Mean.AN.u.x)^2 + (AN.u$y-Mean.AN.u.y)^2 + (AN.u$z-Mean.AN.u.z)^2 #eq. 22 part

		var3D.AN.u <- sum(AN.u$centroidesv)/ (nrow(AN.u)-1)


		##############var3D.ST.d


		ST.d$centroidesv <- (ST.d$x-Mean.ST.d.x)^2 + (ST.d$y-Mean.ST.d.y)^2 + (ST.d$z-Mean.ST.d.z)^2 #eq. 22 part

		var3D.ST.d <- sum(ST.d$centroidesv)/ (nrow(ST.d)-1) #eq. 22


		##############var3D.ST.u

		ST.u$centroidesv <- (ST.u$x-Mean.ST.u.x)^2 + (ST.u$y-Mean.ST.u.y)^2 + (ST.u$z-Mean.ST.u.z)^2 #eq. 22 part

		var3D.ST.u <- sum(ST.u$centroidesv)/ (nrow(ST.u)-1)


		###########################
		##############PADO population average distance of all pooled sex organs to the origin
		###########################

		input$origindist <- sqrt(input$x^2 + input$y^2 + input$z^2)

		PADO <- sum(input$origindist)/ nrow(input)


		
		

		##inaccuracy s-morph
		Inac.S.xyz= (sqrt((Mean.AN.u.x-Mean.ST.d.x)^2+(Mean.AN.u.y-Mean.ST.d.y)^2+(Mean.AN.u.z-Mean.ST.d.z)^2))^2 + var3D.AN.u + var3D.ST.d

		##inaccuracy l-morph
		Inac.L.xyz= (sqrt((Mean.AN.d.x-Mean.ST.u.x)^2+(Mean.AN.d.y-Mean.ST.u.y)^2+(Mean.AN.d.z-Mean.ST.u.z)^2))^2 + var3D.AN.d + var3D.ST.u
		
		
		## total intramorph inaccuracy
		TotalIntraInac=Inac.S.xyz + Inac.L.xyz
		Mean2Stand.TotalIntraInac=TotalIntraInac/(PADO^2)
		Inac.perc.L.xyz = 100* Inac.L.xyz / (Inac.S.xyz+Inac.L.xyz)
		Inac.perc.S.xyz = 100* Inac.S.xyz / (Inac.S.xyz+Inac.L.xyz)
		
		OUTPUT<-matrix(c(Inac.L.xyz, Inac.S.xyz, Inac.perc.L.xyz, Inac.perc.S.xyz, TotalIntraInac, Mean2Stand.TotalIntraInac),
			dimnames=list(c("Inac.L", "Inac.S", "Inac.perc.L", "Inac.perc.S", "TotalInacINTRA", "Mean2Stand.TotalInacINTRA"),POPS[i]))
			
		
		if(sanchez)
			{
			input<-input[,-c(match("origindist",colnames(input)))]

			## Sanchez index
			subsetST<-input[input$sexorg=="ST",]
			distST.2 = sqrt((subsetST$x)^2+(subsetST$y)^2+(subsetST$z)^2) # para subset sexorg==ST

			subsetAN<-input[input$sexorg=="AN",]
			distAN.2 = sqrt((subsetAN$x)^2+(subsetAN$y)^2+(subsetAN$z)^2) #  para subset sexorg==AN

			n.2= nrow(input[which(input$morph=="L" & input$sexorg=="ST"),]) # 	num rows morfo==L & sexorg==ST
			m.2= nrow(input[which(input$morph=="S" & input$sexorg=="AN"),]) # num rows morfo==S & sexorg==AN
			p.2= nrow(input[which(input$morph=="L" & input$sexorg=="AN"),]) #num rows morfo==L & sexorg==AN
			q.2= nrow(input[which(input$morph=="S" & input$sexorg=="ST"),]) #m.2= num rows morfo==S & sexorg==ST
			avdistAN.2= mean(distAN.2, na.rm=na.rm)
			avdistST.2= mean(distST.2, na.rm=na.rm)
			Xr.2=mean(c(avdistAN.2,avdistST.2), na.rm=na.rm)

			### For UP level

			par1 <- input[which(input$"morph"=="L" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"morph"=="S" & input$"sexorg"=="AN"),]

			df.2u<-c()
			for (i.2 in 1:nrow(par1))
			for (j.2 in 1:nrow(par2))
				{
				df.2u<-rbind(df.2u,cbind(par1[i.2,],par2[j.2,]))
				}

			colnames(df.2u)<-c("pop_code_mum","morph_mum","IDmum_L","ID_stigma","sexorg_mum", "upstigmaX","upstigmaY","upstigmaZ","pop_code_dad","morph_dad", "IDdad_S", "ID_anther",
			"sexorg_dad","upantherX","upantherY","upantherZ")
			df.2u$dist.2u <- sqrt((df.2u$upstigmaX-df.2u$upantherX)^2+(df.2u$upstigmaY-df.2u$upantherY)^2+(df.2u$upstigmaZ-df.2u$upantherZ)^2)
			df.2u$distX.2u <-  df.2u$dist.2u/Xr.2
			rL.2u <- sum(df.2u$distX.2u, na.rm=na.rm)/(n.2*m.2)
			distXrL.2u <- (df.2u$distX.2u-rL.2u)^2
			sdrL.2u <- sqrt(sum(distXrL.2u, na.rm=na.rm)/(n.2*m.2))

			## For DOWN level

			par1 <- input[which(input$"morph"=="S" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"morph"=="L" & input$"sexorg"=="AN"),]

			df.2d<-c()
			for (i.3 in 1:nrow(par1))
			for (j.3 in 1:nrow(par2))
			{
			df.2d<-rbind(df.2d,cbind(par1[i.3,],par2[j.3,]))
			}

			colnames(df.2d)<-c("pop_code_mum","morph_mum","IDmum_S","ID_stigma","sexorg_mum", "downstigmaX","downstigmaY","downstigmaZ","pop_code_dad","morph_dad", "IDdad_L", "ID_anther",
			"sexorg_dad","downantherX","downantherY","downantherZ")

			df.2d$dist.2d <- sqrt((df.2d$downstigmaX-df.2d$downantherX)^2+(df.2d$downstigmaY-df.2d$downantherY)^2+(df.2d$downstigmaZ-df.2d$downantherZ)^2)
			df.2d$distX.2d <- df.2d$dist.2d/Xr.2
			rS.2d <- sum(df.2d$distX.2d, na.rm=na.rm)/(p.2*q.2)
			distXrS.2d <- (df.2d$distX.2d-rS.2d)^2
			sdrS.2d <- sqrt(sum(distXrS.2d, na.rm=na.rm)/(p.2*q.2))  ###corrected sqrt

			### Final estimates

			r.2 <- sqrt(rL.2u^2+rS.2d^2)
			sdr.2 <- (sdrL.2u+sdrS.2d)/2
			R.2 <- 1-(10*r.2*sdr.2)
			out.SA <- matrix(c(r.2,R.2),dimnames=list(c("r","R"),POPS[i]))

			verbSanchez<- c(rL.2u, sdrL.2u, rS.2d, sdrS.2d, r.2, sdr.2, R.2)
			names(verbSanchez)<-c("rL", "sdrL", "rS", "sdrS", "r", "sdr", "R")

			}

			###### OUTPUTS
		ifelse(sanchez,
		OUTPUT <- rbind(matrix(c(nL,nS),dimnames=list(c("nL","nS"),POPS[i])),OUTPUT,out.SA),
		OUTPUT <- rbind(matrix(c(nL,nS),dimnames=list(c("nL","nS"),POPS[i])),OUTPUT)
		)
		
		
		}


###### Trimorphic:

		if(Nmorfos==3)
		if(intramorph==FALSE)
		{

		if(is.na(match("level",colnames(input))))
		break ("Error: level column required")



		nL=nrow(input[which(input$morph=="L"),])/(unique(table(input$ID_indiv)))
			nS=nrow(input[which(input$morph=="S"),])/(unique(table(input$ID_indiv)))
		nM=nrow(input[which(input$morph=="M"),])/(unique(table(input$ID_indiv)))


		ST.u<-subset(input[which(input$morph=="L" & input$sexorg=="ST"),])
		ST.d<-subset(input[which(input$morph=="S" & input$sexorg=="ST"),])
		ST.b<-subset(input[which(input$morph=="M" & input$sexorg=="ST"),])

		AN.u<-subset(input[which(input$morph=="S" & input$sexorg=="AN" | input$morph=="M" & input$sexorg=="AN" ),])
		AN.d<-subset(input[which(input$morph==c("L") & input$sexorg=="AN" | input$morph==c("M") & input$sexorg=="AN"),])
		AN.b<-subset(input[which(input$morph==c("S") & input$sexorg=="AN" | input$morph==c("L") & input$sexorg=="AN"),])



		###########################
		######### means
		###########################


		Mean.ST.u.x=mean(ST.u$x, na.rm=T)
		Mean.AN.u.x=mean(AN.u$x, na.rm=T)
		Mean.ST.u.y=mean(ST.u$y, na.rm=T)
		Mean.AN.u.y=mean(AN.u$y, na.rm=T)
		Mean.ST.u.z=mean(ST.u$z, na.rm=T)
		Mean.AN.u.z=mean(AN.u$z, na.rm=T)

		Mean.ST.d.x=mean(ST.d$x, na.rm=T)
		Mean.AN.d.x=mean(AN.d$x, na.rm=T)
		Mean.ST.d.y=mean(ST.d$y, na.rm=T)
		Mean.AN.d.y=mean(AN.d$y, na.rm=T)
		Mean.ST.d.z=mean(ST.d$z, na.rm=T)
		Mean.AN.d.z=mean(AN.d$z, na.rm=T)

		Mean.ST.b.x=mean(ST.b$x, na.rm=T)
		Mean.AN.b.x=mean(AN.b$x, na.rm=T)
		Mean.ST.b.y=mean(ST.b$y, na.rm=T)
		Mean.AN.b.y=mean(AN.b$y, na.rm=T)
		Mean.ST.b.z=mean(ST.b$z, na.rm=T)
		Mean.AN.b.z=mean(AN.b$z, na.rm=T)



		###########################
		#########variances
		###########################

		##############var3D.AN.d

		AN.d$centroidesv <- (AN.d$x-Mean.AN.d.x)^2 + (AN.d$y-Mean.AN.d.y)^2 + (AN.d$z-Mean.AN.d.z)^2 #eq. 22 part

		var3D.AN.d <- sum(AN.d$centroidesv)/ (nrow(AN.d)-1) #eq. 22


		##############var3D.AN.u

		AN.u$centroidesv <- (AN.u$x-Mean.AN.u.x)^2 + (AN.u$y-Mean.AN.u.y)^2 + (AN.u$z-Mean.AN.u.z)^2 #eq. 22 part

		var3D.AN.u <- sum(AN.u$centroidesv)/ (nrow(AN.u)-1)


		##############var3D.AN.b


		AN.b$centroidesv <- (AN.b$x-Mean.AN.b.x)^2 + (AN.b$y-Mean.AN.b.y)^2 + (AN.b$z-Mean.AN.b.z)^2 #eq. 22 part

		var3D.AN.b <- sum(AN.b$centroidesv)/ (nrow(AN.b)-1)


		##############var3D.ST.d

		ST.d$centroidesv <- (ST.d$x-Mean.ST.d.x)^2 + (ST.d$y-Mean.ST.d.y)^2 + (ST.d$z-Mean.ST.d.z)^2 #eq. 22 part

		var3D.ST.d <- sum(ST.d$centroidesv)/ (nrow(ST.d)-1) #eq. 22


		##############var3D.ST.u

		ST.u$centroidesv <- (ST.u$x-Mean.ST.u.x)^2 + (ST.u$y-Mean.ST.u.y)^2 + (ST.u$z-Mean.ST.u.z)^2 #eq. 22 part

		var3D.ST.u <- sum(ST.u$centroidesv)/ (nrow(ST.u)-1)



		##############var3D.ST.b

		ST.b$centroidesv <- (ST.b$x-Mean.ST.b.x)^2 + (ST.b$y-Mean.ST.b.y)^2 + (ST.b$z-Mean.ST.b.z)^2 #eq. 22 part

		var3D.ST.b <- sum(ST.b$centroidesv)/ (nrow(ST.b)-1)



		###########################
		#########inaccuracy high
		###########################

		Inac.u.xyz= (sqrt((Mean.AN.u.x-Mean.ST.u.x)^2+(Mean.AN.u.y-Mean.ST.u.y)^2+(Mean.AN.u.z-Mean.ST.u.z)^2))^2 + var3D.AN.u + var3D.ST.u

		###########################
		#########inaccuracy low
		###########################

		Inac.d.xyz= (sqrt((Mean.AN.d.x-Mean.ST.d.x)^2+(Mean.AN.d.y-Mean.ST.d.y)^2+(Mean.AN.d.z-Mean.ST.d.z)^2))^2 + var3D.AN.d + var3D.ST.d

		###########################
		#########inaccuracy mid
		###########################

		Inac.b.xyz= (sqrt((Mean.AN.b.x-Mean.ST.b.x)^2+(Mean.AN.b.y-Mean.ST.b.y)^2+(Mean.AN.b.z-Mean.ST.b.z)^2))^2 + var3D.AN.b + var3D.ST.b



		###########################
		##############PADO population average distance of all pooled sex organs to the origin
		###########################

		input$origindist <- sqrt(input$x^2 + input$y^2 + input$z^2)

		PADO <- sum(input$origindist)/ nrow(input)


		###########################
		#########total inaccuracy
		###########################

		TotalInac=Inac.u.xyz + Inac.d.xyz + Inac.b.xyz

		Mean2Stand.TotalInac=TotalInac/(PADO^2)

		Inac.perc.u.xyz=100*Inac.u.xyz/(Inac.u.xyz+Inac.d.xyz+Inac.b.xyz)
		Inac.perc.d.xyz=100*Inac.d.xyz/(Inac.u.xyz+Inac.d.xyz+Inac.b.xyz)
		Inac.perc.b.xyz=100*Inac.b.xyz/(Inac.u.xyz+Inac.d.xyz+Inac.b.xyz)

		OUTPUT<-matrix(c(Inac.u.xyz, Inac.d.xyz, Inac.b.xyz, Inac.perc.u.xyz, Inac.perc.d.xyz, Inac.perc.b.xyz, TotalInac, Mean2Stand.TotalInac),
		dimnames=list(c("Inac.u", "Inac.d", "Inac.b,", "Inac.perc.u", "Inac.perc.d", "Inac.perc.b", "TotalInac", "Mean2Stand.TotalInac"),POPS[i]))


			if(sanchez)
			{
			input<-input[,-c(match("origindist",colnames(input)))]

			subsetST<-input[input$sexorg=="ST",]
			distST.3 = sqrt((subsetST$x)^2+(subsetST$y)^2+(subsetST$z)^2) 
			subsetAN<-input[input$sexorg=="AN",]
			distAN.3 = sqrt((subsetAN$x)^2+(subsetAN$y)^2+(subsetAN$z)^2)


			n.3= nrow(input[which(input$level=="up" & input$sexorg=="ST"),]) 
			m.3= nrow(input[which(input$level=="up" & input$sexorg=="AN"),]) 
			p.3= nrow(input[which(input$level=="low" & input$sexorg=="AN"),]) 
			q.3= nrow(input[which(input$level=="low" & input$sexorg=="ST"),]) 
			t.3= nrow(input[which(input$level=="bw" & input$sexorg=="AN"),]) 
			v.3= nrow(input[which(input$level=="bw" & input$sexorg=="ST"),])
			avdistAN.3= mean(distAN.3)
			avdistST.3= mean(distST.3)
			Xr.3=mean(c(avdistAN.3,avdistST.3))

			## Level UP
			par1 <- input[which(input$"level"=="up" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"level"=="up" & input$"sexorg"=="AN"),]

			df.3u<-c()
			for (i in 1:nrow(par1))
			for (j in 1:nrow(par2))
			{
				df.3u<-rbind(df.3u,cbind(par1[i,],par2[j,]))
			}

			colnames(df.3u)<-c("pop_code_mum","morph_mum","level","IDmum_up","ID_stigma","sexorg_mum", "upstigmaX","upstigmaY","upstigmaZ","pop_code_dad","morph_dad","level", "IDdad_up", "ID_anther",
							"sexorg_dad","upantherX","upantherY","upantherZ")

			df.3u$dist.3u <- sqrt((df.3u$upstigmaX-df.3u$upantherX)^2+(df.3u$upstigmaY-df.3u$upantherY)^2+(df.3u$upstigmaZ-df.3u$upantherZ)^2)
			df.3u$distX.3u <-  df.3u$dist.3u/Xr.3
			rUP.3u <- sum(df.3u$distX.3u)/(n.3*m.3)
			distXrUP.3u <- (df.3u$distX.3u-rUP.3u)^2
			sdrUP.3u <- sqrt(sum(distXrUP.3u)/(n.3*m.3))

			### Level DOWN

			par1 <- input[which(input$"level"=="low" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"level"=="low" & input$"sexorg"=="AN"),]

			df.3d<-c()
			for (i in 1:nrow(par1))
			for (j in 1:nrow(par2))
			{
				df.3d<-rbind(df.3d,cbind(par1[i,],par2[j,]))
			}

			colnames(df.3d)<-c("pop_code_mum","morph_mum","level","IDmum_low","ID_stigma","sexorg_mum", "downstigmaX","downstigmaY","downstigmaZ","pop_code_dad","morph_dad","level", "IDdad_low", "ID_anther",
							"sexorg_dad","downantherX","downantherY","downantherZ")


			df.3d$dist.3d <- sqrt((df.3d$downstigmaX-df.3d$downantherX)^2+(df.3d$downstigmaY-df.3d$downantherY)^2+(df.3d$downstigmaZ-df.3d$downantherZ)^2)
			df.3d$distX.3d <- df.3d$dist.3d/Xr.3
			rLOW.3d <- sum(df.3d$distX.3d)/(p.3*q.3)
			distXrLOW.3d <- (df.3d$distX.3d-rLOW.3d)^2
			sdrLOW.3d <- sqrt(sum(distXrLOW.3d)/(p.3*q.3))  

		## Level BETWEEN

			par1 <- input[which(input$"level"=="bw" & input$"sexorg"=="ST"),]
			par2 <- input[which(input$"level"=="bw" & input$"sexorg"=="AN"),]

			df.3b<-c()
			for (i in 1:nrow(par1))
			for (j in 1:nrow(par2))
			{
				df.3b<-rbind(df.3b,cbind(par1[i,],par2[j,]))
			}

			colnames(df.3b)<-c("pop_code_mum","morph_mum","level","IDmum_bw","ID_stigma","sexorg_mum", "bwstigmaX","bwstigmaY","bwstigmaZ","pop_code_dad","morph_dad","level", "IDdad_bw", "ID_anther",
							"sexorg_dad","bwantherX","bwantherY","bwantherZ")

			df.3b$dist.3b <- sqrt((df.3b$bwstigmaX-df.3b$bwantherX)^2+(df.3b$bwstigmaY-df.3b$bwantherY)^2+(df.3b$bwstigmaZ-df.3b$bwantherZ)^2)
			df.3b$distX.3b <- df.3b$dist.3b/Xr.3
			rBW.3b <- sum(df.3b$distX.3b)/(t.3*v.3)
			distXrBW.3b <- (df.3b$distX.3b-rBW.3b)^2
			sdrBW.3b <- sqrt(sum(distXrBW.3b)/(t.3*v.3))  

			### Final estimate

			r.3a <- sqrt(rUP.3u^2+rLOW.3d^2)
			r.3b <- sqrt(rUP.3u^2+rBW.3b^2)
			r.3c <- sqrt(rBW.3b^2+rLOW.3d^2)
			r.3 <- (r.3a + r.3b + r.3c)/3
			sdr.3 <- (sdrUP.3u + sdrLOW.3d + sdrBW.3b)/3 
			R.3 <- 1-(10*r.3*sdr.3)
			out.SA <- matrix(c(r.3,R.3),dimnames=list(c("r","R"),POPS[i]))


			verbSanchez<- c(rUP.3u, sdrUP.3u, rLOW.3d, sdrLOW.3d, rBW.3b, sdrBW.3b, r.3, sdr.3, R.3)
			names(verbSanchez)<-c("rUP", "sdrUP", "rLOW", "sdrLOW", "rBW", "sdrBW", "r", "sdr", "R")
			}

		###### OUTPUTS

				ifelse(sanchez,
		OUTPUT <- rbind(matrix(c(nL,nS,nM),dimnames=list(c("nL","nS","nM"),POPS[i])),OUTPUT,out.SA),
		OUTPUT <- rbind(matrix(c(nL,nS,nM),dimnames=list(c("nL","nS","nM"),POPS[i])),OUTPUT)
		)

		}else{ ## This is for intramorph==TRUE
		
		ST.L<-subset(input[which(input$morph=="L" & input$sexorg=="ST"),])
		ST.S<-subset(input[which(input$morph=="S" & input$sexorg=="ST"),])
		ST.M<-subset(input[which(input$morph=="M" & input$sexorg=="ST"),])

		AN.L<-subset(input[which(input$morph=="L" & input$sexorg=="AN"),])
		AN.S<-subset(input[which(input$morph=="S" & input$sexorg=="AN"),])
		AN.M<-subset(input[which(input$morph=="M" & input$sexorg=="AN"),])


		###########################
		######### means
		###########################


		Mean.ST.L.x=mean(ST.L$x, na.rm=T)
		Mean.AN.L.x=mean(AN.L$x, na.rm=T)
		Mean.ST.L.y=mean(ST.L$y, na.rm=T)
		Mean.AN.L.y=mean(AN.L$y, na.rm=T)
		Mean.ST.L.z=mean(ST.L$z, na.rm=T)
		Mean.AN.L.z=mean(AN.L$z, na.rm=T)

		Mean.ST.S.x=mean(ST.S$x, na.rm=T)
		Mean.AN.S.x=mean(AN.S$x, na.rm=T)
		Mean.ST.S.y=mean(ST.S$y, na.rm=T)
		Mean.AN.S.y=mean(AN.S$y, na.rm=T)
		Mean.ST.S.z=mean(ST.S$z, na.rm=T)
		Mean.AN.S.z=mean(AN.S$z, na.rm=T)

		Mean.ST.M.x=mean(ST.M$x, na.rm=T)
		Mean.AN.M.x=mean(AN.M$x, na.rm=T)
		Mean.ST.M.y=mean(ST.M$y, na.rm=T)
		Mean.AN.M.y=mean(AN.M$y, na.rm=T)
		Mean.ST.M.z=mean(ST.M$z, na.rm=T)
		Mean.AN.M.z=mean(AN.M$z, na.rm=T)



		###########################
		#########variances
		###########################

		##############var3D.AN.S

		AN.S$centroidesv <- (AN.S$x-Mean.AN.S.x)^2 + (AN.S$y-Mean.AN.S.y)^2 + (AN.S$z-Mean.AN.S.z)^2 #eq. 22 part

		var3D.AN.S <- sum(AN.S$centroidesv)/ (nrow(AN.S)-1) #eq. 22


		##############var3D.AN.L

		AN.L$centroidesv <- (AN.L$x-Mean.AN.L.x)^2 + (AN.L$y-Mean.AN.L.y)^2 + (AN.L$z-Mean.AN.L.z)^2 #eq. 22 part

		var3D.AN.L <- sum(AN.L$centroidesv)/ (nrow(AN.L)-1)


		##############var3D.AN.M

		AN.M$centroidesv <- (AN.M$x-Mean.AN.M.x)^2 + (AN.M$y-Mean.AN.M.y)^2 + (AN.M$z-Mean.AN.M.z)^2 #eq. 22 part

		var3D.AN.M <- sum(AN.M$centroidesv)/ (nrow(AN.M)-1)


		##############var3D.ST.S

		ST.S$centroidesv <- (ST.S$x-Mean.ST.S.x)^2 + (ST.S$y-Mean.ST.S.y)^2 + (ST.S$z-Mean.ST.S.z)^2 #eq. 22 part

		var3D.ST.S <- sum(ST.S$centroidesv)/ (nrow(ST.S)-1) #eq. 22


		##############var3D.ST.L

		ST.L$centroidesv <- (ST.L$x-Mean.ST.L.x)^2 + (ST.L$y-Mean.ST.L.y)^2 + (ST.L$z-Mean.ST.L.z)^2 #eq. 22 part

		var3D.ST.L <- sum(ST.L$centroidesv)/ (nrow(ST.L)-1)


		##############var3D.ST.M

		ST.M$centroidesv <- (ST.M$x-Mean.ST.M.x)^2 + (ST.M$y-Mean.ST.M.y)^2 + (ST.M$z-Mean.ST.M.z)^2 #eq. 22 part

		var3D.ST.M <- sum(ST.M$centroidesv)/ (nrow(ST.M)-1)


		###########################
		#########inaccuracy s-morph
		###########################

		Inac.S.xyz= (sqrt((Mean.AN.S.x-Mean.ST.S.x)^2+(Mean.AN.S.y-Mean.ST.S.y)^2+(Mean.AN.S.z-Mean.ST.S.z)^2))^2 + var3D.AN.S + var3D.ST.S

		###########################
		#########inaccuracy l-morph
		###########################

		Inac.L.xyz= (sqrt((Mean.AN.L.x-Mean.ST.L.x)^2+(Mean.AN.L.y-Mean.ST.L.y)^2+(Mean.AN.L.z-Mean.ST.L.z)^2))^2 + var3D.AN.L + var3D.ST.L

		###########################
		#########inaccuracy m-morph
		###########################

		Inac.M.xyz= (sqrt((Mean.AN.M.x-Mean.ST.M.x)^2+(Mean.AN.M.y-Mean.ST.M.y)^2+(Mean.AN.M.z-Mean.ST.M.z)^2))^2 + var3D.AN.M + var3D.ST.M


		###########################
		##############PADO population average distance of all pooled sex organs to the origin
		###########################

		input$origindist <- sqrt(input$x^2 + input$y^2 + input$z^2)

		PADO <- sum(input$origindist)/ nrow(input)


		######################################################
		#########   total 3M intramorph inaccuracy   #########
		######################################################

		Total3MIntraInac=Inac.S.xyz + Inac.L.xyz + Inac.M.xyz

		Mean2Stand.Total3MIntraInac=Total3MIntraInac/(PADO^2)

		Inac.perc.L.xyz = 100* Inac.L.xyz / (Inac.S.xyz+Inac.L.xyz+Inac.M.xyz)
		Inac.perc.S.xyz = 100* Inac.S.xyz / (Inac.S.xyz+Inac.L.xyz+Inac.M.xyz)
		Inac.perc.M.xyz = 100* Inac.M.xyz / (Inac.S.xyz+Inac.L.xyz+Inac.M.xyz)

		OUTPUT<-matrix(c(Inac.L.xyz, Inac.S.xyz, Inac.M.xyz, Inac.perc.L.xyz, Inac.perc.S.xyz, Inac.perc.M.xyz, Total3MIntraInac, Mean2Stand.Total3MIntraInac),
			dimnames=list(c("Inac.L", "Inac.S",  "Inac.M","Inac.perc.L", "Inac.perc.S", "Inac.perc.M", "TotalInacINTRA", "Mean2Stand.TotalInacINTRA"),POPS[i]))
		
		}
		
	OUTPUT_FINAL <- cbind(OUTPUT_FINAL,OUTPUT)
		
	}

OUTPUT_FINAL<-t(round(OUTPUT_FINAL,3))
row.names(OUTPUT_FINAL)<-POPS

 if(verbose)
{
values<-eval(parse(text=paste(paste("c(",paste(ls()[grep("Mean.ST.u",ls())],collapse=",")),")")))
val.names<-ls()[grep("Mean.ST.u",ls())]

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Mean.AN.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Mean.AN.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Mean.ST.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Mean.ST.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Mean.AN.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Mean.AN.d",ls())])

values<-c(values,mean(input$x,na.rm=T))
val.names<-c(val.names,"Mean.ALL.x")

values<-c(values,mean(input$y,na.rm=T))
val.names<-c(val.names,"Mean.ALL.y")

values<-c(values,mean(input$z,na.rm=T))
val.names<-c(val.names,"Mean.ALL.z")

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.ST.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.ST.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.AN.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.AN.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.ST.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.ST.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.AN.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.AN.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Inac.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Inac.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Inac.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Inac.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Inac.perc.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Inac.perc.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Inac.perc.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Inac.perc.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("MAB2.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("MAB2.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("MAB2.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("MAB2.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.perc.AN.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.perc.AN.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.perc.AN.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.perc.AN.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.perc.ST.u",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.perc.ST.u",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("var.perc.ST.d",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("var.perc.ST.d",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("^TotalInac",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("^TotalInac",ls())])

values<-c(values,eval(parse(text=paste(paste("c(",paste(ls()[grep("Mean2Stand.TotalInac",ls())],collapse=",")),")"))))
val.names<-c(val.names,ls()[grep("Mean2Stand.TotalInac",ls())])

 OUTPUT_FINAL<-list(Result=OUTPUT_FINAL,Armbruster=matrix(values,dimnames=list(val.names,c())))

if(sanchez)
	{
	OUTPUT_FINAL[[length(OUTPUT_FINAL)+1]]<-verbSanchez
	names(OUTPUT_FINAL)[[length(OUTPUT_FINAL)]] <- "Sanchez"
	}
}

 return(OUTPUT_FINAL)
}
