/**
 * Copyright (c) 2014-2024 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable.outgoing.cxf.transport;

import static org.ow2.petals.flowable.FlowableSEConstants.Flowable.PETALS_SENDER_COMP_NAME;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.Fault;
import javax.jbi.messaging.MessagingException;

import org.apache.cxf.Bus;
import org.apache.cxf.common.logging.LogUtils;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.transport.AbstractConduit;
import org.apache.cxf.ws.addressing.EndpointReferenceType;
import org.flowable.engine.impl.context.Context;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.util.SourceUtil;
import org.w3c.dom.Document;
import org.w3c.dom.Element;
import org.w3c.dom.Node;

import com.ebmwebsourcing.easycommons.stream.EasyByteArrayOutputStream;
import com.ebmwebsourcing.easycommons.xml.DOMHelper;
import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

public class PetalsConduit extends AbstractConduit implements AsyncCallback {

    private static final Logger LOG = LogUtils.getL7dLogger(PetalsConduit.class);

    public static final ThreadLocal<FlowAttributes> flowAttributes = new ThreadLocal<>();

    private final Bus bus;

    private final AbstractListener sender;

    final AsyncCallback asyncCallback;

    public PetalsConduit(final EndpointReferenceType t, final Bus bus) {
        super(t);
        this.bus = bus;

        // TODO: Use a context providing from CXF and not providing from Flowable
        final Map<Object, Object> beans = Context.getProcessEngineConfiguration().getBeans();
        this.sender = (AbstractListener) beans.get(PETALS_SENDER_COMP_NAME);

        this.asyncCallback = this;

    }

    @Override
    public void prepare(final Message message) throws IOException {
        final NormalizedMessageOutputStream out = new NormalizedMessageOutputStream(this.sender, message, this,
                this.asyncCallback, flowAttributes.get());
        message.setContent(OutputStream.class, out);
    }

    @Override
    protected Logger getLogger() {
        return LOG;
    }

    @Override
    public void onMessage(final org.ow2.petals.component.framework.api.message.Exchange asyncExchange,
            final Exchange cxfExchange) {

        final MessageImpl msg = new MessageImpl();

        if (asyncExchange.isFaultMessage()) {
            // Faults received from the NMR are SOAP Fault
            final Fault fault = asyncExchange.getFault();
            try {
                // TODO: MUST be optimized using directly XML message instead of SOAP message though CXF and Flowable
                // using an XML binding definition in the service WSDL. Waiting, we must wrapped the reply into a SOAP
                // Envelope.
                final Document xmlPayload = wrapAsSoapFault(fault);

                final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
                DOMHelper.prettyPrint(xmlPayload, ebaos);

                if (LOG.isLoggable(Level.FINE)) {
                    LOG.fine("Fault XML payload received: " + ebaos.toString());
                }

                // TODO: Add support for attachments

                msg.setContent(InputStream.class, ebaos.toByteArrayInputStream());
                cxfExchange.setInMessage(msg);

            } catch (final PEtALSCDKException e) {
                // TODO: The error should be pushed into CXF exchange
                LOG.log(Level.WARNING, "An error occurs", e);
            }
        } else if (asyncExchange.isErrorStatus()) {
            // TODO: The error is pushed into CXF exchange as a standard fault
            LOG.log(Level.WARNING, String.format("An error occurs on exchange '%s'", asyncExchange.getExchangeId()),
                    asyncExchange.getError());

            final Document xmlPayload = wrapAsSoapCommonFault(asyncExchange.getError());

            final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
            DOMHelper.prettyPrint(xmlPayload, ebaos);

            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine("Error received as common soap fault: " + ebaos.toString());
            }

            msg.setContent(InputStream.class, ebaos.toByteArrayInputStream());
            cxfExchange.setInMessage(msg);
        } else if (asyncExchange.isDoneStatus()
                && (MEPPatternConstants.ROBUST_IN_ONLY.equals(asyncExchange.getPattern())
                        || MEPPatternConstants.IN_ONLY.equals(asyncExchange.getPattern()))) {

            // TODO: Should be optimized using directly CXF API to avoid the DOM tree hack
            final Document xmlPayload = createEmptySoapBody();

            final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
            DOMHelper.prettyPrint(xmlPayload, ebaos);

            if (LOG.isLoggable(Level.FINE)) {
                LOG.fine("Status DONE received and transformed into: " + ebaos.toString());
            }

            msg.setContent(InputStream.class, ebaos.toByteArrayInputStream());
            cxfExchange.setInMessage(msg);
        } else {
            // TODO: MUST be optimized using directly XML message instead of SOAP message though CXF and Flowable using
            // an XML binding definition in the service WSDL. Waiting, we must wrapped the reply into a SOAP Envelope.
            try {
                final Document outMsgDoc = asyncExchange.getOutMessageContentAsDocument();

                final Document xmlPayload = wrapAsSoapBody(outMsgDoc);

                final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
                DOMHelper.prettyPrint(xmlPayload, ebaos);

                if (LOG.isLoggable(Level.FINE)) {
                    LOG.fine("Output XML payload received: " + ebaos.toString());
                }

                // TODO: Add support for attachments

                msg.setContent(InputStream.class, ebaos.toByteArrayInputStream());
                cxfExchange.setInMessage(msg);

            } catch (final MessagingException e) {
                // TODO: The error should be pushed into CXF exchange
                LOG.log(Level.WARNING, "An error occurs", e);
            }
        }

        this.incomingObserver.onMessage(msg);
    }

    @Override
    public void onExpiredMessage(final org.ow2.petals.component.framework.api.message.Exchange asyncExchange,
            final Exchange cxfExchange) {

        // A timeout occurs, the error is pushed into CXF exchange as a standard fault
        final String errorMsg = String.format("A timeout occurs on exchange '%s'", asyncExchange.getExchangeId());
        LOG.warning(errorMsg);

        final Document xmlPayload = wrapAsSoapCommonFault(new Exception(errorMsg));

        final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
        DOMHelper.prettyPrint(xmlPayload, ebaos);

        if (LOG.isLoggable(Level.FINE)) {
            LOG.fine("Timeout received as common soap fault: " + ebaos.toString());
        }

        final MessageImpl msg = new MessageImpl();
        msg.setContent(InputStream.class, ebaos.toByteArrayInputStream());
        cxfExchange.setInMessage(msg);
        this.incomingObserver.onMessage(msg);

    }

    private static Document wrapAsSoapCommonFault(final Exception jbiError) {

        final Document xmlPayload = DocumentBuilders.newDocument();
        final Element envelope = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/",
                "soap:Envelope");
        final Element body = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "soap:Body");
        final Element fault = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "soap:Fault");
        xmlPayload.appendChild(envelope).appendChild(body).appendChild(fault);

        final Element faultCode = xmlPayload.createElementNS(null, "faultcode");
        faultCode.setTextContent("soap:Server");
        fault.appendChild(faultCode);
        final Element faultString = xmlPayload.createElementNS(null, "faultstring");
        final StringWriter sw = new StringWriter();
        try (final PrintWriter pw = new PrintWriter(sw)) {
            jbiError.printStackTrace(pw);
        }
        faultString.setTextContent(sw.toString());
        fault.appendChild(faultString);

        return xmlPayload;
    }

    public static Document wrapAsSoapFault(final Fault jbiFault) throws PEtALSCDKException {

        final Document jbiFaultDoc = SourceUtil.createDocument(jbiFault.getContent(), false);

        final Document xmlPayload = DocumentBuilders.newDocument();
        final Element envelope = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/",
                "soap:Envelope");
        final Element body = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "soap:Body");
        final Element fault = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "soap:Fault");
        xmlPayload.appendChild(envelope).appendChild(body).appendChild(fault);

        final Element faultCode = xmlPayload.createElementNS(null, "faultcode");
        faultCode.setTextContent("soap:Client");
        fault.appendChild(faultCode);
        final Element faultDetails = xmlPayload.createElementNS(null, "detail");
        final Node importedPayload = xmlPayload.importNode(jbiFaultDoc.getDocumentElement(), true);
        faultDetails.appendChild(importedPayload);
        fault.appendChild(faultDetails);

        return xmlPayload;
    }

    private static Document wrapAsSoapBody(final Document jbiDoc) {

        final Document xmlPayload = DocumentBuilders.newDocument();
        final Element envelope = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "Envelope");
        final Element body = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "Body");
        final Node importedPayload = xmlPayload.importNode(jbiDoc.getDocumentElement(), true);
        xmlPayload.appendChild(envelope).appendChild(body).appendChild(importedPayload);

        return xmlPayload;
    }

    private static Document createEmptySoapBody() {

        final Document xmlPayload = DocumentBuilders.newDocument();
        final Element envelope = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "Envelope");
        final Element body = xmlPayload.createElementNS("http://schemas.xmlsoap.org/soap/envelope/", "Body");
        xmlPayload.appendChild(envelope).appendChild(body);

        return xmlPayload;
    }

}
