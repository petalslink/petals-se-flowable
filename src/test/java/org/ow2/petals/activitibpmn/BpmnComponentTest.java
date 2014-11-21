/**
 * Copyright (c) 2014 Linagora
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
package org.ow2.petals.activitibpmn;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.GregorianCalendar;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.datatype.DatatypeFactory;
import javax.xml.namespace.QName;
import javax.xml.transform.Source;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.junit.Test;
import org.ow2.easywsdl.wsdl.api.abstractItf.AbsItfOperation;
import org.ow2.petals.activitibpmn.operation.exception.NoProcessInstanceIdValueException;
import org.ow2.petals.activitibpmn.operation.exception.NoUserIdValueException;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.Component;
import org.ow2.petals.component.framework.junit.ResponseMessage;
import org.ow2.petals.component.framework.junit.impl.ComponentConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration.ServiceType;
import org.ow2.petals.component.framework.junit.impl.message.WrappedRequestToProviderMessage;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTestBuilder;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;
import org.ow2.petals.se.activitibpmn._1_0.su.Demande;
import org.ow2.petals.se.activitibpmn._1_0.su.Numero;
import org.ow2.petals.se.activitibpmn._1_0.su.Validation;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;
import com.ebmwebsourcing.easycommons.xml.SourceHelper;

/**
 * Unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public class BpmnComponentTest {

    @ClassRule
    public static final ComponentUnderTestBuilder componentBuilder = new ComponentUnderTestBuilder();

    @ClassRule
    public static final InMemoryLogHandler inMemoryLogHandler = new InMemoryLogHandler();

    private static Marshaller marshaller;

    private static Unmarshaller unmarshaller;

    private static Component componentUnderTest;

    private static ServiceConfiguration serviceConfiguration;

    static {
        try {
            JAXBContext context = JAXBContext.newInstance(Demande.class, Validation.class, Numero.class);
            BpmnComponentTest.unmarshaller = context.createUnmarshaller();
            BpmnComponentTest.marshaller = context.createMarshaller();
            BpmnComponentTest.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @BeforeClass
    public static void setup() throws Exception {
        BpmnComponentTest.initializeLoggingSystem();
        BpmnComponentTest.initializeAnsStartComponentUnderTest();
        BpmnComponentTest.deployServiceUnits();
    }

    /**
     * Initialize the logging system reading the configuration defined in resource '/logging.properties'.
     */
    private static void initializeLoggingSystem() throws SecurityException, IOException {
        final InputStream inputStream = ActivitiSuManagerTest.class.getResourceAsStream("/logging.properties");
        assertNotNull("Logging configuration file not found", inputStream);
        try {
            LogManager.getLogManager().readConfiguration(inputStream);
        } finally {
            inputStream.close();
        }
    }

    /**
     * <p>
     * Initialize and start the component under test.
     * </p>
     * <p>
     * The configuration of the component is its default configuration, except the log level set to {@link Leve#FINE}.
     * </p>
     */
    private static void initializeAnsStartComponentUnderTest() throws Exception {

        final Logger logger = Logger.getAnonymousLogger();
        logger.addHandler(BpmnComponentTest.inMemoryLogHandler.getHandler());
        logger.setLevel(Level.FINE);
        BpmnComponentTest.componentUnderTest = BpmnComponentTest.componentBuilder.create(new ComponentConfiguration(
                "componentUnderTest", logger));
        BpmnComponentTest.componentUnderTest.start();
    }

    /**
     * Deploy service units
     */
    private static void deployServiceUnits() {

        final URL wsdlUrl = Thread.currentThread().getContextClassLoader().getResource("su/valid/vacationRequest.wsdl");
        assertNotNull("WSDl not found", wsdlUrl);
        BpmnComponentTest.serviceConfiguration = new ServiceConfiguration(BpmnComponentTest.class.getSimpleName(),
                new QName("http://petals.ow2.org/samples/se-bpmn", "demandeDeConges"), new QName(
                        "http://petals.ow2.org/samples/se-bpmn", "demandeDeCongesService"), "testEndpointName",
                ServiceType.PROVIDE, wsdlUrl);

        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/vacationRequest.bpmn20.xml");
        assertNotNull("BPMN file not found", bpmnUrl);
        BpmnComponentTest.serviceConfiguration.addResource(bpmnUrl);
        BpmnComponentTest.serviceConfiguration.setParameter(
                "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}process_file",
                "vacationRequest.bpmn20.xml");
        BpmnComponentTest.serviceConfiguration.setParameter(
                "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}version", "1");

        BpmnComponentTest.componentUnderTest.installService(BpmnComponentTest.serviceConfiguration);
    }

    @AfterClass
    public static void shutdown() {
        BpmnComponentTest.componentUnderTest.uninstallAllServices();
        BpmnComponentTest.componentUnderTest.shutdown();
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd valid request is sent to complete the waiting user task.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, the user task is ended.</li>
     * </ul>
     * </p>
     */
    @Test
    public void validStartEventRequest() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_1)), null));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("valideur");
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "validerDemande"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_2)), null));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_2 = responseMsg_2.getFault();
        assertNull("Unexpected fault", (fault_2 == null ? null : SourceHelper.toString(fault_2)));
        assertNotNull("No XML payload in response", responseMsg_2.getPayload());
        final Object responseObj_2 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_2.getPayload());
        assertTrue(responseObj_2 instanceof Numero);
        final Numero response_2 = (Numero) responseObj_2;
        assertEquals(response_1.getNumeroDde(), response_2.getNumeroDde());
        // TODO: Add a check against Activiti
    }

    /**
     * <p>
     * Check the message processing where a request is sent to create a new process instance, where:
     * <ul>
     * <li>the user identifier is missing into the incoming payload.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>a fault is returned to the caller</li>
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_NoUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request)), null));

        // Assert the response of the request
        final ResponseMessage responseMsg = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final String faultStr = SourceHelper.toString(fault);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg.getPayload());
    }

    /**
     * <p>
     * Check the message processing where a request is sent to create a new process instance, where:
     * <ul>
     * <li>the user identifier is empty into the incoming payload.</li>
     * </ul>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>a fault is returned to the caller</li>
     * </ul>
     * </p>
     */
    @Test
    public void startEventRequest_EmptyUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request = new Demande();
        request.setDemandeur("");
        request.setNbJourDde(10);
        request.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request.setMotifDde("hollidays");

        // Send the request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request)), null));

        // Assert the response of the request
        final ResponseMessage responseMsg = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault = responseMsg.getFault();
        assertNotNull("No fault returns", fault);
        final String faultStr = SourceHelper.toString(fault);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg.getPayload());
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the user identifier is missing.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_1)), null));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "validerDemande"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_2)), null));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the user identifier is empty.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyUserIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_1)), null));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("");
        request_2.setNumeroDde(response_1.getNumeroDde());
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "validerDemande"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_2)), null));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault", faultStr.contains(NoUserIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the process instance identifier is missing.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_NoProcessInstanceIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_1)), null));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("valideur");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "validerDemande"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_2)), null));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault: " + faultStr, faultStr.contains(NoProcessInstanceIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
    }

    /**
     * <p>
     * Check the message processing where:
     * <ol>
     * <li>a first valid request is sent to create a new process instance,</li>
     * <li>a 2nd request is sent to complete the waiting user task where the process instance identifier is empty.</li>
     * </ol>
     * </p>
     * <p>
     * Expected results:
     * <ul>
     * <li>on the first request, the process instance is correctly created,</li>
     * <li>on the 2nd request, a fault is returned.</li>
     * </ul>
     * </p>
     */
    @Test
    public void userTaskRequest_EmptyProcessInstanceIdValue() throws Exception {

        // Create the 1st valid request
        final Demande request_1 = new Demande();
        request_1.setDemandeur("demandeur");
        request_1.setNbJourDde(10);
        request_1.setDateDebutDde(DatatypeFactory.newInstance().newXMLGregorianCalendar(new GregorianCalendar()));
        request_1.setMotifDde("hollidays");

        // Send the 1st valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "demanderConges"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_1)), null));

        // Assert the response of the 1st valid request
        final ResponseMessage responseMsg_1 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_1 = responseMsg_1.getFault();
        assertNull("Unexpected fault", (fault_1 == null ? null : SourceHelper.toString(fault_1)));
        assertNotNull("No XML payload in response", responseMsg_1.getPayload());
        final Object responseObj_1 = BpmnComponentTest.unmarshaller.unmarshal(responseMsg_1.getPayload());
        assertTrue(responseObj_1 instanceof Numero);
        final Numero response_1 = (Numero) responseObj_1;
        assertNotNull(response_1.getNumeroDde());
        // TODO: Add a check to verify that the process instance exists in Activiti

        // Create the 2nd valid request
        final Validation request_2 = new Validation();
        request_2.setValideur("demandeur");
        request_2.setNumeroDde("");
        request_2.setApprobation(Boolean.TRUE.toString());

        // Send the 2nd valid request
        BpmnComponentTest.componentUnderTest.pushRequestToProvider(new WrappedRequestToProviderMessage(
                BpmnComponentTest.serviceConfiguration, new QName("http://petals.ow2.org/samples/se-bpmn",
                        "validerDemande"), AbsItfOperation.MEPPatternConstants.IN_OUT.value(),
                new ByteArrayInputStream(this.toByteArray(request_2)), null));

        // Assert the response of the 2nd valid request
        final ResponseMessage responseMsg_2 = BpmnComponentTest.componentUnderTest.pollResponseFromProvider();

        final Source fault_2 = responseMsg_2.getFault();
        assertNotNull("No fault returns", fault_2);
        final String faultStr = SourceHelper.toString(fault_2);
        assertTrue("Unexpected fault: " + faultStr, faultStr.contains(NoProcessInstanceIdValueException.class.getName()));
        assertNull("XML payload in response", responseMsg_2.getPayload());
        // TODO: Add a check against Activiti to verify that the user task is not complete
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    private byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            BpmnComponentTest.marshaller.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }

}
