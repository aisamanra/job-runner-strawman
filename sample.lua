-- a basic job:
register {
	  name = "job one",
	  ret  = "this",
	  cmd  = "run-one",
}

-- a job that depends on the first one
register {
	  name = "job two",
	  ret  = "that",
	  deps = { "job one" },
	  cmd  = function (dep)
 		 return "run-two of " .. dep
 	  end
}

-- yet another job, that depends on both the
-- other two!
register {
	  name = "job three",
	  ret  = "done",
	  deps = { "job one", "job two" },
	  cmd  = function(d1, d2)
		 return "both " .. d1 .. " and " .. d2
	  end
}
