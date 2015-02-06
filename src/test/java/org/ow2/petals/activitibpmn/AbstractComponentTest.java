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
package org.ow2.petals.activitibpmn;

import static org.junit.Assert.assertNotNull;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_PORT_TYPE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.IntegrationOperation.ITG_SERVICE;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.logging.LogManager;
import java.util.logging.Logger;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;
import javax.xml.namespace.QName;

import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.ClassRule;
import org.ow2.petals.commons.log.Level;
import org.ow2.petals.component.framework.junit.Component;
import org.ow2.petals.component.framework.junit.impl.ComponentConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration;
import org.ow2.petals.component.framework.junit.impl.ServiceConfiguration.ServiceType;
import org.ow2.petals.component.framework.junit.rule.ComponentUnderTestBuilder;
import org.ow2.petals.components.activiti.generic._1.GetTasks;
import org.ow2.petals.components.activiti.generic._1.GetTasksResponse;
import org.ow2.petals.junit.rules.log.handler.InMemoryLogHandler;
import org.ow2.petals.samples.se_bpmn.archivageservice.Archiver;
import org.ow2.petals.samples.se_bpmn.archivageservice.ArchiverResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.AckResponse;
import org.ow2.petals.samples.se_bpmn.vacationservice.Demande;
import org.ow2.petals.samples.se_bpmn.vacationservice.DemandeDejaValidee;
import org.ow2.petals.samples.se_bpmn.vacationservice.Numero;
import org.ow2.petals.samples.se_bpmn.vacationservice.NumeroDemandeInconnu;
import org.ow2.petals.samples.se_bpmn.vacationservice.Validation;

import com.ebmwebsourcing.easycommons.lang.UncheckedException;

/**
 * Abstract class for unit tests about request processing
 * 
 * @author Christophe DENEUX - Linagora
 * 
 */
public abstract class AbstractComponentTest {

    private static final String VACATION_NAMESPACE = "http://petals.ow2.org/samples/se-bpmn/vacationService";

    protected static final QName VACATION_INTERFACE = new QName(VACATION_NAMESPACE, "demandeDeConges");

    protected static final QName VACATION_SERVICE = new QName(VACATION_NAMESPACE, "demandeDeCongesService");

    protected static final String VACATION_ENDPOINT = "testEndpointName";

    protected static final QName OPERATION_DEMANDERCONGES = new QName(VACATION_NAMESPACE, "demanderConges");

    protected static final QName OPERATION_VALIDERDEMANDE = new QName(VACATION_NAMESPACE, "validerDemande");

    private static final String ARCHIVE_NAMESPACE = "http://petals.ow2.org/samples/se-bpmn/archivageService";

    protected static final QName ARCHIVE_INTERFACE = new QName(ARCHIVE_NAMESPACE, "archiver");

    protected static final QName ARCHIVE_SERVICE = new QName(ARCHIVE_NAMESPACE, "archiverService");

    protected static final String ARCHIVE_ENDPOINT = "archiveEndpointName";

    protected static final QName ARCHIVER_OPERATION = new QName(ARCHIVE_NAMESPACE, "archiver");

    @ClassRule
    public static final ComponentUnderTestBuilder componentBuilder = new ComponentUnderTestBuilder();

    @ClassRule
    public static final InMemoryLogHandler inMemoryLogHandler = new InMemoryLogHandler();

    private static Marshaller marshaller;

    protected static Unmarshaller unmarshaller;

    /**
     * The Petals SE Activiti to test
     */
    protected static Component componentUnderTest;

    protected static ServiceConfiguration serviceConfiguration;

    /**
     * The integration service provided by the component
     */
    protected static ServiceConfiguration nativeServiceConfiguration;

    static {
        try {
            final JAXBContext context = JAXBContext.newInstance(Demande.class, Validation.class, Numero.class,
                    AckResponse.class, NumeroDemandeInconnu.class, DemandeDejaValidee.class, Archiver.class,
                    ArchiverResponse.class, GetTasks.class, GetTasksResponse.class);
            AbstractComponentTest.unmarshaller = context.createUnmarshaller();
            AbstractComponentTest.marshaller = context.createMarshaller();
            AbstractComponentTest.marshaller.setProperty(Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE);
        } catch (final JAXBException e) {
            throw new UncheckedException(e);
        }
    }

    @BeforeClass
    public static void setup() throws Exception {
        AbstractComponentTest.initializeLoggingSystem();
        AbstractComponentTest.initializeAnsStartComponentUnderTest();
        AbstractComponentTest.deployServiceUnits();

        AbstractComponentTest.registerOtherServiceProviders();
    }

    @AfterClass
    public static void shutdown() {
        AbstractComponentTest.componentUnderTest.uninstallAllServices();
        AbstractComponentTest.componentUnderTest.shutdown();
    }

    /**
     * All log traces must be cleared before starting a unit test
     */
    @Before
    public void clearLogTraces() {
        inMemoryLogHandler.clear();
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
        logger.addHandler(AbstractComponentTest.inMemoryLogHandler.getHandler());
        logger.setLevel(Level.FINE);
        AbstractComponentTest.componentUnderTest = AbstractComponentTest.componentBuilder
                .create(new ComponentConfiguration("componentUnderTest", logger));
        AbstractComponentTest.componentUnderTest.start();

        final URL nativeServiceWsdlUrl = Thread.currentThread().getContextClassLoader().getResource("component.wsdl");
        assertNotNull("Integration servce WSDl not found", nativeServiceWsdlUrl);
        AbstractComponentTest.nativeServiceConfiguration = new ServiceConfiguration(
                AbstractComponentTest.class.getSimpleName(), ITG_PORT_TYPE, ITG_SERVICE,
                AbstractComponentTest.componentUnderTest.getNativeEndpointName(ITG_SERVICE), ServiceType.PROVIDE,
                nativeServiceWsdlUrl);
    }

    /**
     * Deploy service units
     */
    private static void deployServiceUnits() {

        final URL wsdlUrl = Thread.currentThread().getContextClassLoader().getResource("su/valid/vacationRequest.wsdl");
        assertNotNull("WSDl not found", wsdlUrl);
        AbstractComponentTest.serviceConfiguration = new ServiceConfiguration(
                AbstractComponentTest.class.getSimpleName(), VACATION_INTERFACE, VACATION_SERVICE, VACATION_ENDPOINT,
                ServiceType.PROVIDE, wsdlUrl);

        final URL demanderCongesResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/demanderCongesResponse.xsl");
        assertNotNull("Output XSL 'demanderCongesResponse.xsl' not found", demanderCongesResponseXslUrl);
        AbstractComponentTest.serviceConfiguration.addResource(demanderCongesResponseXslUrl);

        final URL validerDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/validerDemandeResponse.xsl");
        assertNotNull("Output XSL 'validerDemandeResponse.xsl' not found", validerDemandeResponseXslUrl);
        AbstractComponentTest.serviceConfiguration.addResource(validerDemandeResponseXslUrl);

        final URL ajusterDemandeResponseXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/ajusterDemandeResponse.xsl");
        assertNotNull("Output XSL 'ajusterDemandeResponse.xsl' not found", ajusterDemandeResponseXslUrl);
        AbstractComponentTest.serviceConfiguration.addResource(ajusterDemandeResponseXslUrl);

        final URL numeroDemandeInconnuXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/numeroDemandeInconnu.xsl");
        assertNotNull("Output XSL 'numeroDemandeInconnu.xsl' not found", numeroDemandeInconnuXslUrl);
        AbstractComponentTest.serviceConfiguration.addResource(numeroDemandeInconnuXslUrl);

        final URL demandeDejaValideeXslUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/demandeDejaValidee.xsl");
        assertNotNull("Output XSL 'demandeDejaValidee.xsl' not found", demandeDejaValideeXslUrl);
        AbstractComponentTest.serviceConfiguration.addResource(demandeDejaValideeXslUrl);

        final URL bpmnUrl = Thread.currentThread().getContextClassLoader()
                .getResource("su/valid/vacationRequest.bpmn20.xml");
        assertNotNull("BPMN file not found", bpmnUrl);
        AbstractComponentTest.serviceConfiguration.addResource(bpmnUrl);
        AbstractComponentTest.serviceConfiguration.setParameter(
                "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}process_file",
                "vacationRequest.bpmn20.xml");
        AbstractComponentTest.serviceConfiguration.setParameter(
                "{http://petals.ow2.org/components/petals-se-activitibpmn/version-1.0}version", "1");

        // Consume service 'archiver'
        // TODO: The consume section seems mandatory to retrieve the consume endpoint on async exchange between Activti
        // and other services
        final ServiceConfiguration consumeServiceConfiguration = new ServiceConfiguration(
                AbstractComponentTest.class.getSimpleName(), ARCHIVE_INTERFACE, ARCHIVE_SERVICE, ARCHIVE_ENDPOINT,
                ServiceType.CONSUME);
        AbstractComponentTest.serviceConfiguration.addServiceConfigurationDependency(consumeServiceConfiguration);

        AbstractComponentTest.componentUnderTest.installService(AbstractComponentTest.serviceConfiguration);
    }

    private static void registerOtherServiceProviders() {
        componentBuilder.registerOtherServiceProvider(ARCHIVE_SERVICE, ARCHIVE_ENDPOINT);
    }

    /**
     * Convert a JAXB element to bytes
     * 
     * @param jaxbElement
     *            The JAXB element to write as bytes
     */
    protected byte[] toByteArray(final Object jaxbElement) throws JAXBException, IOException {

        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        try {
            AbstractComponentTest.marshaller.marshal(jaxbElement, baos);
            return baos.toByteArray();
        } finally {
            baos.close();
        }
    }

}
