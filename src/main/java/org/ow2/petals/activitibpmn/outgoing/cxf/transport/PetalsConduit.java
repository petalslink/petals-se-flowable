/**
 * Copyright (c) 2014-2015 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn.outgoing.cxf.transport;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.Activiti.PETALS_SENDER_COMP_NAME;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;
import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamResult;

import org.activiti.engine.impl.context.Context;
import org.apache.cxf.Bus;
import org.apache.cxf.common.logging.LogUtils;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.transport.AbstractConduit;
import org.apache.cxf.ws.addressing.EndpointReferenceType;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;
import com.ebmwebsourcing.easycommons.xml.Transformers;

public class PetalsConduit extends AbstractConduit implements AsyncCallback {

    private static final Logger LOG = LogUtils.getL7dLogger(PetalsConduit.class);

    public static ThreadLocal<FlowAttributes> flowAttributes = new ThreadLocal<FlowAttributes>();

    private final Bus bus;

    private final AbstractListener sender;

    final AsyncCallback asyncCallback;

    public PetalsConduit(final EndpointReferenceType t, final Bus bus) {
        super(t);
        this.bus = bus;

        // TODO: Use a context providing from CXF and not providing from Activiti
        final Map<Object, Object> beans = Context.getProcessEngineConfiguration().getBeans();
        this.sender = (AbstractListener) beans.get(PETALS_SENDER_COMP_NAME);

        this.asyncCallback = this;

    }

    @Override
    public void prepare(final Message message) throws IOException {

        try {
            final NormalizedMessageOutputStream out = new NormalizedMessageOutputStream(this.sender,
                    message.getExchange(), this.asyncCallback, flowAttributes.get());
            message.setContent(OutputStream.class, out);
        } catch (final MessagingException e) {
            throw new IOException(e);
        } catch (final PEtALSCDKException e) {
            throw new IOException(e);
        }
    }

    @Override
    protected Logger getLogger() {
        return LOG;
    }

    @Override
    public void onMessage(final org.ow2.petals.component.framework.api.message.Exchange asyncExchange,
            final Exchange cxfExchange) {

        final MessageImpl msg = new MessageImpl();

        final Fault fault = asyncExchange.getFault();
        if (fault == null) {
            // TODO: MUST be optimized using directly XML message instead of SOAP message though CXF and Activiti using
            // an XML binding definition in the service WSDL. Waiting, we must wrapped the reply into a SOAP Envelope.
            try {
                final Document outMsgDoc = asyncExchange.getOutMessageContentAsDocument();

                final Document xmlPayload = DocumentBuilders.newDocument();
                final Element envelope = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/",
                        "Envelope");
                final Element body = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "Body");
                final Node importedPayload = xmlPayload.importNode(outMsgDoc.getDocumentElement(), true);
                xmlPayload.appendChild(envelope).appendChild(body).appendChild(importedPayload);

                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                com.ebmwebsourcing.easycommons.xml.DOMHelper.prettyPrint(xmlPayload, baos);

                // TODO: Change log level to FINE
                LOG.info("Outgoing XML payload: " + new String(baos.toByteArray()));

                // TODO: Add support for attachments

                msg.setContent(InputStream.class, new ByteArrayInputStream(baos.toByteArray()));
                cxfExchange.setInMessage(msg);

            } catch (final MessagingException e) {
                // TODO: The error should be pushed into CXF exchange
                LOG.log(Level.WARNING, "An error occurs", e);
            }
        } else {
            // TODO: Add a unit test
            // Faults received from the NMR are SOAP Fault
            try {
                final Source faultSource = fault.getContent();
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                final Result result = new StreamResult(baos);
                final Transformer transformer = Transformers.takeTransformer();
                try {
                    transformer.transform(faultSource, result);
                } finally {
                    Transformers.releaseTransformer(transformer);
                }

                msg.setContent(InputStream.class, new ByteArrayInputStream(baos.toByteArray()));
                cxfExchange.setInFaultMessage(msg);
            } catch (final TransformerException e) {
                // TODO: The error should be pushed into CXF exchange
                LOG.log(Level.WARNING, "An error occurs", e);
            }
        }

        incomingObserver.onMessage(msg);
    }

    @Override
    public void onExpiredMessage(final org.ow2.petals.component.framework.api.message.Exchange asyncExchange,
            Exchange cxfExchange) {
        // NOP: As we had set the same timeout at CXF client level than the timeout at NMR level, we have nothing to do
        // because the timeout at CXF client level has fired in the same time.
    }

}
