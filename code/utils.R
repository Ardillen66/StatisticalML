#Utility functions
library(nnet)
library(rpart) 

#Function retrieved from exercise session 6 of the course statistical foundations of machine learning
replace_na_with_mean_value<-function(vec) {
    mean_vec<-mean(vec,na.rm=T)
    vec[is.na(vec)]<-mean_vec
    vec
}

loo_estimates_nn<-function(x, y, size=2, maxit=1000){
	n_samples<-NROW(x)
	estimates = numeric(n_samples)
	for (s in 1:n_samples) {
		leave_out_x<-x[s,]

		train_x<-x[-s,]
		train_y<-y[-s]

		model<-nnet(train_x, train_y, size=size, maxit=maxit, trace=FALSE, linout=TRUE)

		estimates[s]<-predict(model, leave_out_x)

	}
	estimates
}

loo_estimates_pol<-function(x, y, degree=1){
	n_samples<-NROW(x)
	estimates = numeric(n_samples)
	for (s in 1:n_samples) {
		leave_out_x<-x[s,]

		train_x<-x[-s,]
		train_y<-y[-s]

		xp_train<-NULL
		xp_test<-NULL
		for (deg in 1:degree) {
			xp_train<-cbind(xp_train, train_x^deg)
			xp_test<-cbind(xp_test, leave_out_x^deg)
		}
		ds <- data.frame(cbind(train_y, xp_train))
		names(ds)[1]<-"sale_price"

		model<-lm(sale_price~., ds)

		test_data<-data.frame(xp_test)
 		names(test_data)<-names(ds[2:NCOL(ds)])

		estimates[s]<-predict(model, test_data)

	}
	estimates
}

loo_estimates_tree<-function(x, y, prune=TRUE){
	n_samples<-NROW(x)
	estimates = numeric(n_samples)
	for (s in 1:n_samples) {
		leave_out_x<-x[s,]

		train_x<-x[-s,]
		train_y<-y[-s]

		ds<-cbind(train_x, sale_price=train_y)
		#Fit tree
		fit<-rpart(sale_price~., method="anova", data=ds)
		if(prune){
			#Find complexity parameter with lowest cv error
			cp<-fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
			#prune with found parameter
			fit<-prune(fit, cp=cp)
		}
		estimates[s]<-predict(fit, leave_out_x)	

	}
	estimates
}

