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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;

import org.apache.cxf.message.Exchange;
import org.apache.cxf.service.model.EndpointInfo;
import org.ow2.petals.activitibpmn.outgoing.PetalsActivitiAsyncContext;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.component.framework.api.Message.MEPConstants;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.generated.Consumes;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.ebmwebsourcing.easycommons.xml.DocumentBuilders;

/**
 * An {@link OutputStream} to write bytes into the normalized message 'IN' of a {@link MessageExchange}. The
 * {@link MessageExchange} is automatically sent when closing the {@link OutputStream}.
 * 
 * @author Christophe DENEUX - Linagora
 */
// TODO: NormalizedMessageOutputStream should extend an output stream similar to
// org.apache.camel.converter.stream.CachedOutputStream to avoid memory problems
public class NormalizedMessageOutputStream extends ByteArrayOutputStream {

    private final AbstractListener sender;

    private final Exchange cxfExchange;

    private final AsyncCallback asyncCallback;

    private final FlowAttributes flowAttributes;

    public NormalizedMessageOutputStream(final AbstractListener sender, final Exchange cxfExchange,
            final AsyncCallback asyncCallback, final FlowAttributes flowAttributes) throws MessagingException,
            PEtALSCDKException {
        this.sender = sender;
        this.cxfExchange = cxfExchange;
        this.asyncCallback = asyncCallback;
        this.flowAttributes = flowAttributes;
    }

    @Override
    public void close() throws IOException {
        super.close();

        final EndpointInfo endpointInfo = this.cxfExchange.getEndpoint().getEndpointInfo();
        final QName interfaceName = endpointInfo.getInterface().getName();
        final QName serviceName = endpointInfo.getService().getName();
        // TODO: Find a way to define the endpoint name to use.
        final QName operationName = this.cxfExchange.getBindingOperationInfo().getOperationInfo().getName();

        try {
            final Consumes consume = new Consumes();
            // TODO: Create a unit test where the interface name is missing
            consume.setInterfaceName(interfaceName);
            // TODO: Create a unit test where the service name is missing
            consume.setServiceName(serviceName);
            // TODO: Find a way to define the endpoint name to use.
            // TODO: Create a unit test where the endpoint name is missing
            // TODO: Create a unit test where the operation name is missing
            consume.setOperation(operationName);
            // TODO: Find a way to define the MEP to use.
            final org.ow2.petals.component.framework.api.message.Exchange exchange = this.sender.createConsumeExchange(
                    consume, MEPConstants.IN_OUT_PATTERN);
            ((org.ow2.petals.component.framework.message.ExchangeImpl) exchange).getMessageExchange()
                    .setFlowAttributes(this.flowAttributes);

            // TODO: Add support for attachments
            // TODO: MUST be optimized generating directly an XML message by CXF or Activiti
            // The buffer contains a SOAP message, we just remove the SOAP enveloppe
            final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
            try {
                this.sender.getLogger().fine("Request to send: " + new String(buf));
                final Document doc = docBuilder.parse(new ByteArrayInputStream(buf));
                final NodeList soapBodies = doc.getElementsByTagNameNS("http://schemas.xmlsoap.org/soap/envelope/",
                        "Body");
                final Node xmlPayload = soapBodies.item(0).getFirstChild();
                final ByteArrayOutputStream baos = new ByteArrayOutputStream();
                com.ebmwebsourcing.easycommons.xml.DOMHelper.prettyPrint(xmlPayload, baos);
                exchange.setInMessageContent(new ByteArrayInputStream(baos.toByteArray()));
                // exchange.setInMessageContent(new ByteArrayInputStream(buf));
            } catch (final SAXException e) {
                throw new IOException(e);
            } finally {
                DocumentBuilders.releaseDocumentBuilder(docBuilder);
            }

            // TODO: Set the TTL of the async context
            // TODO: Add MONIT trace
            this.sender.sendAsync(exchange, new PetalsActivitiAsyncContext(exchange, this.cxfExchange,
                    this.asyncCallback));
        } catch (final MessagingException e) {
            throw new IOException(e);
        } catch (final PEtALSCDKException e) {
            throw new IOException(e);
        }
    }
}
