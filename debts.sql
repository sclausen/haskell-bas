SELECT 
  SUM(stock.price),
  stock.ownerId,
  user.username
FROM purchase
LEFT JOIN user  ON user.id  = purchase.userId
LEFT JOIN stock ON stock.id = purchase.stockId
GROUP BY stock.ownerId, user.username
ORDER BY user.username