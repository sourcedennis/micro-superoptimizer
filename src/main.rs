// stdlib imports
use std::fmt::Display;
// external library imports
use fxhash::FxHashSet;


#[derive(Clone, Debug, PartialEq)]
enum BinOp {
  And, Or, Xor, Shl, Shr, Add, Mul
}

impl BinOp {
  fn is_comm( &self ) -> bool {
    *self != BinOp::Shl && *self != BinOp::Shr
  }
}

#[derive(Clone, Debug, PartialEq)]
enum UnOp {
  Not
}

#[derive(Clone, Debug, PartialEq)]
enum Var {
  X, Y
}

#[derive(Clone, PartialEq)]
enum Term {
  Const( u8 ),
  Var( Var ),
  UnOp( UnOp, Box< Term > ),
  BinOp( BinOp, Box< Term >, Box< Term > )
}


const SPEC_INPUT: [(u8,u8); 9] =
  [ (0b01,0b01), (0b01,0b10), (0b01,0b11)
  , (0b10,0b01), (0b10,0b10), (0b10,0b11)
  , (0b11,0b01), (0b11,0b10), (0b11,0b11)
  ];

struct Spec( [u8; 9] );

// We can characterize a term by its "fingerprint", representing its outputs on
// each of the inputs in `SPEC_INPUT`.
type Fingerprint = [u8; 9];

impl Term {
  fn unop( op: UnOp, t: Term ) -> Term {
    Term::UnOp( op, Box::new( t ) )
  }

  fn binop( op: BinOp, x: Term, y: Term ) -> Term {
    Term::BinOp( op, Box::new( x ), Box::new( y ) )
  }

  fn eval( &self, x: u8, y: u8 ) -> u8 {
    match self {
      Term::Const( v ) => *v,
      Term::Var( Var::X ) => x,
      Term::Var( Var::Y ) => y,
      Term::UnOp( UnOp::Not, t ) => !t.eval( x, y ),
      Term::BinOp( BinOp::And, t1, t2 ) => t1.eval( x, y ) &  t2.eval( x, y ),
      Term::BinOp( BinOp::Or,  t1, t2 ) => t1.eval( x, y ) |  t2.eval( x, y ),
      Term::BinOp( BinOp::Xor, t1, t2 ) => t1.eval( x, y ) ^  t2.eval( x, y ),
      Term::BinOp( BinOp::Shl, t1, t2 ) => t1.eval( x, y ) << t2.eval( x, y ),
      Term::BinOp( BinOp::Shr, t1, t2 ) => t1.eval( x, y ) >> t2.eval( x, y ),
      Term::BinOp( BinOp::Add, t1, t2 ) => t1.eval( x, y ) +  t2.eval( x, y ),
      Term::BinOp( BinOp::Mul, t1, t2 ) => t1.eval( x, y ) *  t2.eval( x, y )
    }
  }

  fn fingerprint( &self ) -> Fingerprint {
    let mut fp: [u8; 9] = [0; 9];
    for i in 0..9 {
      let (x, y) = SPEC_INPUT[ i ];
      fp[ i ] = self.eval( x, y );
    }
    fp
  }
}

impl Display for Term {
  fn fmt( &self, f: &mut std::fmt::Formatter<'_> ) -> std::fmt::Result {
    fn fmt_bracket(
      t: &Term,
      needs_bracket: bool,
      f: &mut std::fmt::Formatter<'_>
    ) -> std::fmt::Result {
      match t {
        Term::Const( v ) => write!( f, "{}", v ),
        Term::Var( Var::X ) => write!( f, "x" ),
        Term::Var( Var::Y ) => write!( f, "y" ),
        Term::UnOp( UnOp::Not, t ) => {
          write!( f, "!" )?;
          fmt_bracket( t, true, f )
        },
        Term::BinOp( op, t1, t2 ) => {
          if needs_bracket {
            write!( f, "(" )?;
          }
          fmt_bracket( t1, true, f )?;
          match op {
            BinOp::And => write!( f, " & " )?,
            BinOp::Or  => write!( f, " | " )?,
            BinOp::Xor => write!( f, " ^ " )?,
            BinOp::Shl => write!( f, " << " )?,
            BinOp::Shr => write!( f, " >> " )?,
            BinOp::Add => write!( f, " + " )?,
            BinOp::Mul => write!( f, " * " )?,
          }
          fmt_bracket( t2, true, f )?;
          if needs_bracket {
            write!( f, ")" )?;
          }
          Ok( () )
        }
      }
    }
    fmt_bracket( self, false, f )
  }
}

impl Spec {
  /// Finds an input for which the given `Term` *does not* satisfy this
  /// specification, if it exists. `None` otherwise.
  pub fn counterexample( &self, t: &Term ) -> Option< (u8, u8) > {
    for i in 0..9 {
      let (x, y) = SPEC_INPUT[ i ];
      if t.eval( x, y ) != self.0[ i ] {
        return Some( (x, y) );
      }
    }
    None
  }
}

/// Constructs a `Vec` with all `Term`s of size `n`, which is built from smaller
/// terms provided in `terms`. `Term`s whose fingerprint was previously
/// encountered (by `seen_fps`) are never produced.
fn enumerate_terms(
  seen_fps: &mut FxHashSet< Fingerprint >,
  terms: &[Vec< Term >],
  n: usize
) -> Vec< Term > {
  let mut out = Vec::new( );
  if n == 1 {
    out.push( Term::Var( Var::X ) );
    out.push( Term::Var( Var::Y ) );
    // we only generate any of these 3 constants
    out.push( Term::Const( 0b01 ) );
    out.push( Term::Const( 0b10 ) );
    out.push( Term::Const( 0b11 ) );
  } else {
    if n >= 2 { // unops fit
      let operand_n = n - 1;
      for x in &terms[ operand_n - 1 ] {
        let new_term = Term::unop( UnOp::Not, x.clone( ) );
        if seen_fps.insert( new_term.fingerprint( ) ) {
          out.push( new_term );
        } // otherwise: we've already seen a semantically-equivalent term
      }
    }
    if n >= 3 { // bin ops fit
      for lhs_n in 1..=n-2 {
        let rhs_n = n - lhs_n - 1;

        for x in &terms[ lhs_n - 1 ] {
          for y in &terms[ rhs_n - 1 ] {
            for op in [BinOp::And, BinOp::Or, BinOp::Xor,
                        BinOp::Shl, BinOp::Shr, BinOp::Add, BinOp::Mul] {

              if !op.is_comm( ) || lhs_n >= rhs_n {
                let new_term = Term::binop( op, x.clone( ), y.clone( ) );
                if seen_fps.insert( new_term.fingerprint( ) ) {
                  out.push( new_term );
                } // otherwise: we've already seen a semantically-equivalent term
              }
            }
          }
        }
      }
    }
  }
  out
}

fn main( ) {
  // Our specification table (for inputs in `SPEC_INPUT`)
  let spec = Spec(
    [ 0b01, 0b01, 0b01
    , 0b11, 0b01, 0b11
    , 0b11, 0b01, 0b11
    ] );

  let mut seen_fingerprints = FxHashSet::default( );
  let mut terms = Vec::new( );
  // Don't try `Term`s larger than 15
  for n in 1..=15 {
    let new_terms = enumerate_terms( &mut seen_fingerprints, &terms, n );
    println!( "Generated {} terms of size {}", new_terms.len( ), n );

    for t in &new_terms {
      if spec.counterexample( t ).is_none( ) {
        println!( "Found term: {}", t );
        return;
      }
    }
    terms.push( new_terms );
  }
}
