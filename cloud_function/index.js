'use strict';

require('dotenv').config();
const line = require('@line/bot-sdk');
const datastore = require('@google-cloud/datastore')({
  projectId: process.env.GCP_PROJECT
});

const config = {
  channelAccessToken: process.env.CHANNEL_ACCESS_TOKEN,
  channelSecret: process.env.CHANNEL_SECRET,
};

const client = new line.Client(config);
const backetName = process.env.BACKET_NAME;

function handleEvent(event) {
  console.log(event);
  if (event.type === 'follow' && event.source.type === 'user') {
    const entity = {
      key:  datastore.key([backetName, backetName + event.source.userId]),
      data: { mid: event.source.userId }
    };
    return datastore.upsert(entity);
  } else if (event.type === 'unfollow' && event.source.type === 'user') {
    return datastore.delete(datastore.key([backetName, backetName + event.source.userId]));
  } else {
    return Promise.resolve(null);
  }
}

exports.handler = function echoBot (req, res) {
  Promise
    .all(req.body.events.map(handleEvent))
    .then(result => res.status(200).send(`Success: ${result}`))
    .catch(err => { console.log(err.originalError.response.data) ; res.status(400).send(err.toString()) });
};
