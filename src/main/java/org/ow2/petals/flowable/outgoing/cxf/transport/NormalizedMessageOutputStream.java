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

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.jbi.messaging.MessageExchange;
import javax.jbi.messaging.MessagingException;
import javax.xml.namespace.QName;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.transform.dom.DOMSource;

import org.apache.cxf.common.logging.LogUtils;
import org.apache.cxf.endpoint.ClientImpl;
import org.apache.cxf.message.Exchange;
import org.apache.cxf.message.Message;
import org.apache.cxf.message.MessageImpl;
import org.apache.cxf.service.model.EndpointInfo;
import org.apache.cxf.service.model.OperationInfo;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation.MEPPatternConstants;
import org.ow2.petals.commons.log.FlowAttributes;
import org.ow2.petals.commons.log.PetalsExecutionContext;
import org.ow2.petals.component.framework.jbidescriptor.generated.Consumes;
import org.ow2.petals.component.framework.jbidescriptor.generated.MEPType;
import org.ow2.petals.component.framework.listener.AbstractListener;
import org.ow2.petals.component.framework.logger.StepLogHelper;
import org.ow2.petals.flowable.outgoing.PetalsFlowableAsyncContext;
import org.w3c.dom.Document;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import org.xml.sax.SAXException;

import com.ebmwebsourcing.easycommons.stream.EasyByteArrayOutputStream;
import com.ebmwebsourcing.easycommons.xml.DOMHelper;
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

    private static final Logger LOG = LogUtils.getL7dLogger(NormalizedMessageOutputStream.class);

    /**
     * Complementary part of the CXF timeout {@link ClientImpl#SYNC_TIMEOUT} for the internal processing
     */
    private static final long CXF_SYNC_TIMEOUT_INTERNAL_PART = 10000;

    private final AbstractListener sender;

    private final Message cxfMessage;

    private final PetalsConduit conduit;

    private final AsyncCallback asyncCallback;

    private final FlowAttributes flowAttributes;

    public NormalizedMessageOutputStream(final AbstractListener sender, final Message cxfMessage,
            final PetalsConduit conduit, final AsyncCallback asyncCallback, final FlowAttributes flowAttributes) {
        this.sender = sender;
        this.cxfMessage = cxfMessage;
        this.conduit = conduit;
        this.asyncCallback = asyncCallback;
        this.flowAttributes = flowAttributes;
    }

    @Override
    public void close() throws IOException {
        super.close();

        // needed so that logs are put in the right flow instance folder
        PetalsExecutionContext.putFlowAttributes(this.flowAttributes);

        final Exchange cxfExchange = this.cxfMessage.getExchange();

        final EndpointInfo endpointInfo = cxfExchange.getEndpoint().getEndpointInfo();
        final QName interfaceName = endpointInfo.getInterface().getName();
        final QName service = endpointInfo.getService().getName();
        final OperationInfo operationInfo = cxfExchange.getBindingOperationInfo().getOperationInfo();
        final QName operation = operationInfo.getName();

        try {
            Consumes consume = this.sender.getComponent().getServiceUnitManager().getConsumesFromDestination(null,
                    service, interfaceName, operation);

            final MEPPatternConstants mep = getMEP(operationInfo, consume);

            if (consume == null) {
                this.sender.getLogger().log(Level.WARNING, String.format(
                        "No Consumes declared in the JBI descriptor for the request to send, using informations from the process and default timeout: interface=%s, serviceName=%s, operation=%s, mep=%s",
                        interfaceName, service, operation, mep));
                consume = new Consumes();
                // TODO: Create a unit test where the interface name is missing
                consume.setInterfaceName(interfaceName);
                // TODO: Create a unit test where the service name is missing
                consume.setServiceName(service);
            } else {
                if (interfaceName != null && !consume.getInterfaceName().equals(interfaceName)) {
                    this.sender.getLogger().log(Level.WARNING,
                            "Mismatch between JBI Consumes interface name and process information ("
                                    + consume.getInterfaceName() + " vs " + interfaceName
                                    + "), using Consumes information.");
                }
                if (service != null && !service.equals(consume.getServiceName())) {
                    this.sender.getLogger().log(Level.WARNING,
                            "Mismatch between JBI Consumes service name and process information ("
                                    + consume.getServiceName() + " vs " + service
                                    + "), using Consumes information.");
                }
            }

            // TODO: Find a way to define the endpoint name to use: maybe the address could contain it in endpointInfo?
            // TODO: Create a unit test where the endpoint name is missing
            // TODO: Create a unit test where the operation name is missing

            final org.ow2.petals.component.framework.api.message.Exchange jbiExchange = this.sender
                    .createConsumeExchange(consume, mep);

            // We always use the operation from the process (the JBI Consumes defines the service used, not the
            // operation)
            jbiExchange.setOperation(operation);

            // Set timeout at CXF level. It must be upper than the timeout defined into SU JBI descriptor level)
            final long timeout = this.sender.getTimeout(consume);
            if (timeout > 0) {
                cxfExchange.getOutMessage().put(ClientImpl.SYNC_TIMEOUT,
                        timeout + CXF_SYNC_TIMEOUT_INTERNAL_PART);
            }

            // TODO: Add support for attachments
            // TODO: MUST be optimized generating directly an XML message by CXF or Flowable
            // The buffer contains a SOAP message, we just remove the SOAP envelope
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
                    jbiExchange.setInMessageContent(new DOMSource(xmlPayload));
                } else {
                    throw new IOException("Empty service task request");
                }
            } catch (final SAXException e) {
                throw new IOException(e);
            } finally {
                DocumentBuilders.releaseDocumentBuilder(docBuilder);
            }

            // Pattern RobustInOnly is not supported by WSDL 1.1 and it is considered as InOnly. So we must manage it
            // here manually.
            if (cxfExchange.isOneWay() && mep == MEPPatternConstants.ROBUST_IN_ONLY) {

                // We store here service name, endpoint name and operation because if a timeout occurs we can't get them
                // because we have not the ownership on the JBI exchange
                final String serviceName = jbiExchange.getService() == null
                        ? StepLogHelper.TIMEOUT_ERROR_MSG_UNDEFINED_REF
                        : jbiExchange.getService().toString();
                final String endpointName = jbiExchange.getEndpointName() == null
                        ? StepLogHelper.TIMEOUT_ERROR_MSG_UNDEFINED_REF
                        : jbiExchange.getEndpointName();
                final String operationName = jbiExchange.getOperation() == null
                        ? StepLogHelper.TIMEOUT_ERROR_MSG_UNDEFINED_REF
                        : jbiExchange.getOperation().toString();

                if (this.sender.sendSync(jbiExchange)) {
                    if (jbiExchange.isErrorStatus()) {
                        // An error was returned
                        throw jbiExchange.getError();
                    } else if (jbiExchange.isDoneStatus()) {
                        // Status DONE returned
                    } else {
                        // A fault was returned
                        final Message cxfInMessage = new MessageImpl();
                        cxfInMessage.setExchange(cxfExchange);

                        final Document faultReceived = PetalsConduit.wrapAsSoapFault(jbiExchange.getFault());

                        final EasyByteArrayOutputStream ebaos = new EasyByteArrayOutputStream();
                        DOMHelper.prettyPrint(faultReceived, ebaos);
                        if (LOG.isLoggable(Level.FINE)) {
                            LOG.fine("Fault XML payload received: " + ebaos.toString());
                        }
                        cxfInMessage.setContent(InputStream.class, ebaos.toByteArrayInputStream());

                        this.conduit.getMessageObserver().onMessage(cxfInMessage);
                        jbiExchange.setDoneStatus();
                        this.sender.send(jbiExchange);
                    }
                } else {
                    // A timeout occurs
                    final String timeoutErrorMessage = String.format(StepLogHelper.TIMEOUT_ERROR_MSG_PATTERN, timeout,
                            interfaceName, serviceName, endpointName, operationName,
                            this.flowAttributes.getFlowInstanceId(), this.flowAttributes.getFlowStepId());
                    throw new MessagingException(timeoutErrorMessage);
                }
            } else {
                this.sender.sendAsync(jbiExchange, new PetalsFlowableAsyncContext(cxfExchange, this.asyncCallback));
            }
        } catch (final IOException e) {
            throw e;
        } catch (final Exception e) {
            throw new RuntimeException(e);
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
