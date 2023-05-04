

/* File parser.mly */
        %token <string> VAR
        %token <string> OBJECT
        %token <string> ROLE
        %token LPAREN
        %token RPAREN
        %token TYPE
        %token ARROW
        %token QUERY
        %token AND
        %token EOL
        %start main             /* the entry point */
        %type <Lib.axiom list> main
        
        %%
        
        main:
            QUERY VAR ARROW LPAREN gp RPAREN               {$5}
        ;
        
        gp : 
          clause                       {$1}
          | clause AND gp              { List.append $1 $3 }
        ;

        clause:
            VAR TYPE OBJECT        {($1 , Atomic $3) :: [] }     
          | VAR ROLE OBJECT        {($1 , Exist(Property $2, Atomic $3)) :: []}
          | OBJECT ROLE VAR        {($3 , Exist(PropertyInverse $2, Atomic $1)) :: []}
          | VAR ROLE VAR           {($1 , Exist(Property $2, Atomic $3)) :: 
                                    ($3 , Exist(PropertyInverse $2, Atomic $1)) :: 
                                    []}
        ;

        %%