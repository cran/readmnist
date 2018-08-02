Read.mnist<-function(filename){
	fp=file(filename,"rb")
	magic_number=readBin(fp,integer(),n=1,endian="big")
	Info<-list()
	Info$magic_number<-magic_number
	
	if(magic_number==2049){ #label file
		number=readBin(fp,integer(),n=1,endian="big")
		labels=readBin(fp,integer(),size=1,n=number,endian="big")
		Info$labels<-labels
		if(number==10000){ remark="test_label"
		}else if(number==60000) remark="train_label"	
	}else if(magic_number==2051){ #image file
		number=readBin(fp,integer(),n=1,endian="big")
		nrow=readBin(fp,integer(),n=1,endian="big")
		ncol=readBin(fp,integer(),n=1,endian="big")
		pic<-matrix(readBin(fp,integer(),size=1,n=nrow*ncol*number,endian="big",signed=FALSE),number,nrow*ncol)
		Info$pic<-pic
		Info$nrow<-nrow
		Info$ncol<-ncol
		if(number==10000){ remark="test_image"
		}else if(number==60000) remark="train_image"	
	}
	else{
		print("the file has a unexpected magic number")
		print("please check which is a real mnist dataset")
		Info<-list()
		close(fp)
		return(Info)
	}
	Info$number<-number
	Info$mark<-remark
	close(fp)
	return(Info)
}
