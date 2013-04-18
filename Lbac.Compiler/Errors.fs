module Errors

    type Try<'Success, 'Error> = 
        | Success of 'Success
        | Error of 'Error



