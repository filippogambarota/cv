function Cite(el)
  -- Check if the citation is in NormalCitation mode
  if el.citations[1].mode == 'NormalCitation' or el.citations[1].mode == "AuthorInText" then
    for i, content in ipairs(el.content) do
      if content.t == "Str" and content.text:find("Gambarota") then
        -- Replace "Gambarota" with a bold version
        el.content[i] = pandoc.Strong { pandoc.Str("Gambarota") }
      end
    end
  end
  return el
end
