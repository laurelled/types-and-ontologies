

/* File parser.mly */
        %token <string> VAR
        %token <string> OBJECT
        %token <string> ROLE
        %token <string> INVROLE
        %token LPAREN
        %token RPAREN
        %token TYPE
        %token ARROW
        %token QUERY
        %token AND
        %token EOL
        %start main             /* the entry point */
        %type <Lib.query> main
        
        %%
        
        main:
            QUERY VAR ARROW LPAREN gp RPAREN               {Q (V $2 , $5)}
        ;
        
        gp : 
          clause                       {$1}
          | clause AND gp              { CP ($1 , $3)  }
        ;

        clause:
          | VAR role OBJECT        {SP (V $1 , $2 , O $3)}
          | OBJECT role VAR        {OP (O $1 , $2 , V $3)}
          | VAR role VAR           {DP (V $1 , $2 , V $3)}
        ;

        role :
          TYPE       {Type}
          | ROLE     {P ($1)} 
          | INVROLE  {I ($1)}

        %%