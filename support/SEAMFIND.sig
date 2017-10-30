signature SEAMFIND =
sig

  structure Seq : SEQUENCE

  type gradient = real

  val findSeam : gradient Seq.seq Seq.seq -> int Seq.seq

end
