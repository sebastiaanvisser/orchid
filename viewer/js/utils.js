String.prototype.interpolate =
  function interpolate(vars)                                                                            
  {
    return this.replace(/\$([0-9a-z-A-Z_]+)/g,
      function (m, id)
      {   
        return (typeof vars[id] === "undefined") ? m : vars[id]
      }
    )
  }

String.prototype.asHTML =
  function asHTML ()                                                                            
  {
    return this
      .replace(/\&/g, '&amp;')
      .replace(/</g, '&lt;')
      .replace(/>/g, '&gt;')
  }

