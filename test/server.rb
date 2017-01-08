require 'sinatra'

get '/payments/api/:api' do
  puts request.body
  "Done"
end

post '/payments/api/:api' do
  puts request.body
  "Done"
end
