/**
 * Copyright (c) 2014-2016 Linagora
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
import java.util.logging.Level;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.dom.DOMSource;

import org.apache.cxf.message.Exchange;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.OperationInfo;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.activitibpmn.outgoing.PetalsActivitiAsyncContext;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.commons.log.FlowAttributesExchangeHelper;
import org.ow2.petals.commons.log.PetalsExecutionContext;
import org.ow2.petals.component.framework.api.exception.PEtALSCDKException;
import org.ow2.petals.component.framework.jbidescriptor.generated.Consumes;
import org.ow2.petals.component.framework.jbidescriptor.generated.MEPType;
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


        // needed so that logs are put in the right flow instance folder
        PetalsExecutionContext.putFlowAttributes(this.flowAttributes);

        final EndpointInfo endpointInfo = this.cxfExchange.getEndpoint().getEndpointInfo();
        final QName interfaceName = endpointInfo.getInterface().getName();
        final QName serviceName = endpointInfo.getService().getName();
        final OperationInfo operationInfo = this.cxfExchange.getBindingOperationInfo().getOperationInfo();
        final QName operationName = operationInfo.getName();

        try {
            Consumes consume = this.sender.getComponent().getServiceUnitManager().getConsumesFromDestination(null,
                    serviceName, interfaceName);

            final MEPPatternConstants mep = getMEP(operationInfo, consume);
            
            if (consume == null) {
                this.sender.getLogger().log(Level.WARNING,
                        String.format(
                                "No Consumes declared in the JBI descriptor for the request to send, using informations from the process: interface=%s, serviceName=%s, operation=%s, mep=%s",
                                interfaceName, serviceName, operationName, mep));
                consume = new Consumes();
                // TODO: Create a unit test where the interface name is missing
                consume.setInterfaceName(interfaceName);
                // TODO: Create a unit test where the service name is missing
                consume.setServiceName(serviceName);
            } else {
                if (interfaceName != null && !consume.getInterfaceName().equals(interfaceName)) {
                    this.sender.getLogger().log(Level.WARNING,
                            "Mismatch between JBI Consumes interface name and process information ("
                                    + consume.getInterfaceName() + " vs " + interfaceName
                                    + "), using Consumes information.");
                }
                if (serviceName != null && !consume.getServiceName().equals(serviceName)) {
                    this.sender.getLogger().log(Level.WARNING,
                            "Mismatch between JBI Consumes service name and process information ("
                                    + consume.getServiceName() + " vs " + serviceName
                                    + "), using Consumes information.");
                }
                if (consume.getOperation() != null) {
                    this.sender.getLogger().log(Level.WARNING,
                            "An operation is declared in the Consumes in the JBI descriptor for the request to send: IGNORED and using informations from the process: "
                                    + operationName);
                }
                if (consume.getMep() != null) {
                    this.sender.getLogger().log(Level.WARNING,
                            "A MEP is declared in the Consumes in the JBI descriptor for the request to send: IGNORED and using informations from the process: "
                                    + mep);
                }
            }

            // TODO: Find a way to define the endpoint name to use: maybe the address could contain it in endpointInfo?
            // TODO: Create a unit test where the endpoint name is missing
            // TODO: Create a unit test where the operation name is missing

            final org.ow2.petals.component.framework.api.message.Exchange exchange = this.sender
                    .createConsumeExchange(consume, mep);

            // we always use the operation from the process (the JBI Consumes defines the service used, not the
            // operation)
            exchange.setOperation(operationName);

            FlowAttributesExchangeHelper.setFlowAttributes(
                    ((org.ow2.petals.component.framework.message.ExchangeImpl) exchange).getMessageExchange(),
                    this.flowAttributes);

            // TODO: Add support for attachments
            // TODO: MUST be optimized generating directly an XML message by CXF or Activiti
            // The buffer contains a SOAP message, we just remove the SOAP enveloppe
            final DocumentBuilder docBuilder = DocumentBuilders.takeDocumentBuilder();
            try {
                if (this.sender.getLogger().isLoggable(Level.FINE)) {
                    this.sender.getLogger().fine("Request to send: " + new String(buf));
                }
                final Document doc = docBuilder.parse(new ByteArrayInputStream(buf));
                final NodeList soapBodies = doc.getElementsByTagNameNS("http://schemas.xmlsoap.org/soap/envelope/",
                        "Body");
                if (soapBodies.item(0).hasChildNodes()) {
                    final Node xmlPayload = soapBodies.item(0).getFirstChild();
                    exchange.setInMessageContent(new DOMSource(xmlPayload));
                } else {
                    throw new IOException("Empty service task request");
                }
            } catch (final SAXException e) {
                throw new IOException(e);
            } finally {
                DocumentBuilders.releaseDocumentBuilder(docBuilder);
            }

            // TODO: Set the TTL of the async context
            // TODO: Add MONIT trace
            this.sender.sendAsync(exchange, new PetalsActivitiAsyncContext(this.cxfExchange, this.asyncCallback));
        } catch (final MessagingException e) {
            throw new IOException(e);
        }
    }

    /**
     * TODO should we really use the consume MEP if it does not specify an operation...?!
     */
    private static MEPPatternConstants getMEP(final OperationInfo op, final Consumes consume) {
        if (!op.hasOutput()) {
            // let's consider the consume could be used to force the MEP
            if (op.hasFaults() || (consume != null && consume.getMep() == MEPType.ROBUST_IN_ONLY)) {
                return MEPPatternConstants.ROBUST_IN_ONLY;
            } else {
                return MEPPatternConstants.IN_ONLY;
            }
        } else {
            // TODO not sure how to determine if it's inout or inoptout from the info...
            // but at least we can consider the consume could be used to force it
            if (consume != null && consume.getMep() == MEPType.IN_OPTIONAL_OUT) {
                return MEPPatternConstants.IN_OPTIONAL_OUT;
            } else {
                return MEPPatternConstants.IN_OUT;
            }
        }
    }
}
