##STEP 3
mplusit <- function(labs, dat){
  
  ms <- list()
  m.fits <- list()
  outs <- list()
  
  for(i in 1:length(labs)){
    
    ms[[labs[i]]] <- mplusObject(
      
      TITLE = glue("MplusAuto Step 3 ({labs[i]})"),
      DATA = "listwise=on;",
      
      VARIABLE = glue("
      nominal = N;
      categorical = {labs[i]};
      usevar = N {labs[i]};
      classes = c(5);
    "),
      
      DEFINE = glue("
      if ({labs[i]} eq 1) then {labs[i]} = 0;
      if ({labs[i]} eq 2) then {labs[i]} = 1;
    "),
      
      ANALYSIS = "
      Type = mixture;
      proc = 4 (starts);
      starts = 500 100;
      algorithm = integration;
    ",
      
      MODEL = glue("
      %overall%

      [{labs[i]}$1] (x_thr);

      [C#1]  (int_x0c1);
      [C#2]  (int_x0c2);
      [C#3]  (int_x0c3);
      [C#4]  (int_x0c4);

      C#1 on {labs[i]} (x_eff1);
      C#2 on {labs[i]} (x_eff2);
      C#3 on {labs[i]} (x_eff3);
      C#4 on {labs[i]} (x_eff4);
    
      %C#1%
      [n#1@{logit_cprobs[1,1]}];
      [n#2@{logit_cprobs[1,2]}];
      [n#3@{logit_cprobs[1,3]}];
      [n#4@{logit_cprobs[1,4]}];
  
      %C#2%
      [n#1@{logit_cprobs[2,1]}];
      [n#2@{logit_cprobs[2,2]}];
      [n#3@{logit_cprobs[2,3]}];
      [n#4@{logit_cprobs[2,4]}];
  
      %C#3%
      [n#1@{logit_cprobs[3,1]}];
      [n#2@{logit_cprobs[3,2]}];
      [n#3@{logit_cprobs[3,3]}];
      [n#4@{logit_cprobs[3,4]}];
   
      %C#4%
      [n#1@{logit_cprobs[4,1]}];
      [n#2@{logit_cprobs[4,2]}];
      [n#3@{logit_cprobs[4,3]}];
      [n#4@{logit_cprobs[4,4]}];
      
      %C#5%
      [n#1@{logit_cprobs[5,1]}];
      [n#2@{logit_cprobs[5,2]}];
      [n#3@{logit_cprobs[5,3]}];
      [n#4@{logit_cprobs[5,4]}];
    "),
      
      MODELCONSTRAINT = "
      new (x_prev);
        x_prev = exp((-1)*x_thr)/(1 + exp((-1)*x_thr));

      new (temp_0c1 temp_0c2 temp_0c3 temp_0c4 px0_c1 px0_c2 px0_c3 px0_c4 px0_c5);
        temp_0c1 = exp(int_x0c1);
        temp_0c2 = exp(int_x0c2);
        temp_0c3 = exp(int_x0c3);
        temp_0c4 = exp(int_x0c4);
        px0_c1 = temp_0c1/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c2 = temp_0c2/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c3 = temp_0c3/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c4 = temp_0c4/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);
        px0_c5 = 1/(1 + temp_0c1 + temp_0c2 + temp_0c3 + temp_0c4);

      new (temp_1c1 temp_1c2 temp_1c3 temp_1c4 px1_c1 px1_c2 px1_c3 px1_c4 px1_c5);
        temp_1c1 = exp(int_x0c1 + x_eff1);
        temp_1c2 = exp(int_x0c2 + x_eff2);
        temp_1c3 = exp(int_x0c3 + x_eff3);
        temp_1c4 = exp(int_x0c4 + x_eff4);
        px1_c1 = temp_1c1/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c2 = temp_1c2/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c3 = temp_1c3/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c4 = temp_1c4/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);
        px1_c5 = 1/(1 + temp_1c1 + temp_1c2 + temp_1c3 + temp_1c4);

      new(cell_1 cell_2 cell_3 cell_4 cell_5 cell_6 cell_7 cell_8 cell_9 cell_10);
        cell_1 = (1 - x_prev)*px0_c1;
        cell_2 = (1 - x_prev)*px0_c2;
        cell_3 = (1 - x_prev)*px0_c3;
        cell_4 = (1 - x_prev)*px0_c4;
        cell_5 = (1 - x_prev)*px0_c5;
        cell_6 = x_prev*px1_c1;
        cell_7 = x_prev*px1_c2;
        cell_8 = x_prev*px1_c3;
        cell_9 = x_prev*px1_c4;
        cell_10 = x_prev*px1_c5;
      ",
      
      rdata = dat)
    
    m.fits[[labs[i]]] <- mplusModeler(object=ms[[labs[i]]],
                                      modelout=paste0("5cl_llca_step3_",labs[i], ".inp"), 
                                      run = 1L, hashfilename = FALSE, writeData = 'always')
    
    outs[[labs[i]]] <- m.fits[[i]]$results$parameters$unstandardized[m.fits[[i]]$results$parameters$unstandardized$paramHeader %in% c('New.Additional.Parameters'),c('param','est','se','est_se','pval')]
    
  }
  return(outs)
}

outs <- mplusit(labs[1:50],alldf)
outs <- c(outs, mplusit(labs[51:100],alldf))
outs <- c(outs, mplusit(labs[101:150],alldf))
outs <- c(outs, mplusit(labs[151:200],alldf))
outs <- c(outs, mplusit(labs[201:250],alldf))
outs <- c(outs, mplusit(labs[251:300],alldf))
outs <- c(outs, mplusit(labs[301:350],alldf))
outs <- c(outs, mplusit(labs[351:400],alldf))
outs <- c(outs, mplusit(labs[401:450],alldf))
outs <- c(outs, mplusit(labs[451:500],alldf))
outs <- c(outs, mplusit(labs[501:length(labs)],alldf))
save(outs, file='//rdsfcifs.acrc.bris.ac.uk/MRC-IEU-research/projects/ieu2/p7/025/working/results/outs.rda')
