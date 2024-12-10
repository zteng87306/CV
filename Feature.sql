WITH 
-- CTE: 取得每個玩家的第一場遊戲時間
first_play AS (
    SELECT 
        t023.MemberId AS Player_ID,
        MIN(t023.PlayDate) AS fstPlay
    FROM 
        `uba-pro.uba.t023_player_net_win_summary` AS t023
    LEFT JOIN 
        `uba-pro.uba.t11_player_firstlogin` AS t11
    ON 
        t023.MemberId = t11.MemberId
    WHERE 
        DATE(t11.CreateTime) BETWEEN '2024-05-01' AND '2024-10-31'
        AND DATE(t023.PlayDate) >= '2024-05-01'
    GROUP BY 
        t023.MemberId
),
-- CTE: 取得遊戲紀錄並標記時間差，判斷是否屬於不同的遊玩區段
game_details AS (
    SELECT 
        t5a01.Player_ID, 
        t5a01.PlayerGamePlayDetailRecord_Timestamp AS playtime, 
        LAG(t5a01.PlayerGamePlayDetailRecord_Timestamp) 
            OVER (PARTITION BY t5a01.Player_ID ORDER BY t5a01.PlayerGamePlayDetailRecord_Timestamp) AS lastplaytime,
        t5a01.Hall_ID, 
        t5a01.GameID,
        t5a01.PlayerGamePlayDetailRecord_TotalBet AS CoinIn, 
        t5a01.PlayerGamePlayDetailRecord_TotalWin AS CoinOut, 
        t5a01.PlayerGamePlayDetailRecord_TotalWin / t5a01.PlayerGamePlayDetailRecord_TotalBet AS Multi,
        IF(t5a01.PlayerGamePlayDetailRecord_CardUsed > 0, 1, 0) AS IsCard,
        IF(t5a01.PlayerGamePlayDetailRecord_FreeGameWin > 0, 1, 0) AS FreeGameWin,
        IF(t5a01.PlayerGamePlayDetailRecord_RespinGameWin > 0, 1, 0) AS RespinWin,
        IF(t5a01.PlayerGamePlayDetailRecord_BonusGameWin > 0, 1, 0) AS BonusGameWin,
        CASE
            WHEN 
                t5a01.PlayerGamePlayDetailRecord_FreeGameWin > 0
            OR
                t5a01.PlayerGamePlayDetailRecord_RespinGameWin > 0
            OR
                t5a01.PlayerGamePlayDetailRecord_BonusGameWin > 0
            THEN
                t5a01.PlayerGamePlayDetailRecord_TotalWin / t5a01.PlayerGamePlayDetailRecord_TotalBet
            ELSE
                NULL
        END AS FreeGameWinRate, 
        -- 根據遊戲時間差來標記是否為新一個遊玩區段
        IF(TIMESTAMP_DIFF(t5a01.PlayerGamePlayDetailRecord_Timestamp, 
                          LAG(t5a01.PlayerGamePlayDetailRecord_Timestamp) 
                          OVER (PARTITION BY t5a01.Player_ID ORDER BY t5a01.PlayerGamePlayDetailRecord_Timestamp), 
                          MINUTE) >= 60, 1, 0) AS nextsession
    FROM 
        `uba-pro.uba.t5a01_player_game_play_detail_record_202012041600` AS t5a01
    RIGHT JOIN 
        first_play AS start
    ON 
        t5a01.Player_ID = start.Player_ID
    WHERE 
        t5a01.PlayerGamePlayDetailRecord_Timestamp >= 'TIME SLOT'
        AND DATE_DIFF(DATE(t5a01.PlayerGamePlayDetailRecord_Timestamp), DATE(start.fstPlay), DAY) <= 30
),
-- CTE: 計算每個玩家每個session
sessions AS (
    SELECT 
        Player_ID,
        playtime,
        CoinIn,
        CoinOut,
        Multi,
        IsCard,
        FreeGameWin,
        RespinWin,
        BonusGameWin,
        FreeGameWinRate,
        GameID,
        -- 根據 nextsession 標記每個區段的ID
        SUM(nextsession) OVER (PARTITION BY Player_ID ORDER BY playtime) + 1 AS session
    FROM 
        game_details
)
-- 主查詢
SELECT 
    Player_ID, 
    session, 
    MIN(playtime) AS sessionStart, 
    MAX(playtime) AS sessionEnd,
    COUNT(playtime) AS spins,
    AVG(CoinIn) AS avgbet, 
    SUM(CoinOut) / SUM(CoinIn) AS RTP, 
    AVG(Multi) AS MultiRTP,
    SUM(IsCard) AS CardUses, 
    SUM(IF((FreeGameWin + RespinWin + BonusGameWin) > 0, 1, 0)) / COUNT(playtime) AS FeatureGamesRate,
    AVG(FreeGameWinRate) AS FeatureGamesAvgMulti,
    COUNT(DISTINCT GameID) AS GamesPlayed,
    SUM(CASE WHEN Multi >= 20 THEN 1 ELSE 0 END) / COUNT(playtime) AS BigWinRatio,
    SUM(CASE WHEN Multi >= 50 THEN 1 ELSE 0 END) / COUNT(playtime) AS MegaWinRatio,
    SUM(CASE WHEN Multi >= 100 THEN 1 ELSE 0 END) / COUNT(playtime) AS SuperWinRatio,
    SUM(CASE WHEN Multi >= 300 THEN 1 ELSE 0 END) / COUNT(playtime) AS JumboWinRatio
FROM 
    sessions
GROUP BY 
    1, 2
ORDER BY 
    1, 2