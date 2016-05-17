module Widget.CarbonAds (
    carbonAdsWidget
) where

import Import

carbonAdsWidget :: Widget
carbonAdsWidget = $(widgetFile "widgets/carbon-ads")
