%{
  open Lib
%}

/* File parser.mly */
        %token <string> VAR
        %token <string> OBJECT
        %token <string> ROLE
        %token TYPE
        %token ARROW
        %token QUERY
        %token AND
        %token EOL
        %start main             /* the entry point */
        %type <Lib.axiom list> main
        
        %%
        
        main:
            QUERY VAR ARROW gp EOL                { $4 }
        ;
        gp:
            VAR TYPE OBJECT        {($1 , Atomic $3) :: [] }     
          | VAR ROLE OBJECT        {($1 , Exist(Property $2, Atomic $3)) :: []}
          | OBJECT ROLE VAR        {($3 , Exist(PropertyInverse $2, Atomic $1)) :: []}
          | VAR ROLE VAR           {($1 , Exist(Property $2, Atomic $3)) :: 
                                    ($3 , Exist(PropertyInverse $2, Atomic $1)) :: 
                                    []}
          | gp AND gp              {List.append $1 $3 }
        ;

        %%