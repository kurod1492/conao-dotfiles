sudo ln -s ~/.dotfiles/quicklook/qlimagesize/build/Release/qlImageSize.qlgenerator
sudo ln -s ~/.dotfiles/quicklook/qlmarkdown/build/Release/QLMarkdown.qlgenerator
sudo ln -s ~/.dotfiles/quicklook/qlscript/ScriptQL.qlgenerator 
sudo ln -s ~/.dotfiles/quicklook/qlstephen/QuickLookStephenProject/build/Release/QLStephen.qlgenerator
sudo ln -s ~/.dotfiles/quicklook/quicklookcsv/QuickLookCSV.qlgenerator
sudo ln -s ~/.dotfiles/quicklook/quicklookjson/QuickLookJSON.qlgenerator
defaults write org.n8gray.QLColorCode extraHLFlags '-l'
defaults write org.n8gray.QLColorCode hlTheme seashell  
qlmanage -r

